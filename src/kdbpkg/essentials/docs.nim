import sequtils
import macros
import options
import tables
import sets
import fusion/matching
import hashes
import json # for stringification
import strutils
{.experimental: "caseStmtMacros".}

type
    ID* = distinct string
        ## IDs uniquely identify Docs stored in the database.
        ## IDs MUST start with :
    Doc* = object
        ## Docs are the entry point to your database. Each Doc
        ## is analogous to a single record or database "row".
        ## The "columns" are the Exprs that each Doc contains.
        ##
        ## Docs are typically used in one of two ways:
        ## 1. To store data, or
        ## 2. To define vocab that other Docs can contain.
        ##
        ## Data docs store richly structured data within their
        ## Exprs, much like a JSON document or a s-expression.
        ## Vocab docs give structure to the library by becoming
        ## possible vocabulary within other docs' subexpressions.
        ##
        ## For example, suppose we have a library with the following
        ## five Docs that record a user's vacation:
        ##   ":abcd" -> (title "Trip to Barcelona")
        ##              (Journal (Date "2024-01-01"
        ##                         (Text "The plane was late ..."))
        ##                       (Date "2024-01-05"
        ##                         (Visited "Park Güell")
        ##                         (Text "I can't believe...")))
        ##   ":efgh" -> (title "Journal") (vocabFor ":top")
        ##   ":ijkl" -> (title "Date") (vocabFor ":efgh")
        ##   ":mnop" -> (title "Text") (vocabFor ":ijkl") (vocabFor ":top")
        ##   ":qrst" -> (title "Visited") (vocabFor ":ijkl")
        ##
        ## All five Docs live in the database and can be
        ## edited in the same way. The ":abcd" node contains the
        ## bulk of the data content, while the ":efgh", ":ijkl",
        ## ":mnop", and ":qrst" nodes define the vocab that "abcd"
        ## confirms to. Each of these vocab Docs
        ## has an "vocabFor" Expr pointing to either the ID of
        ## some parent vocab Doc, or ":top" to denote that the
        ## Expr should appear at the top level of the Doc.
        ##
        ## There's no separation between data and vocab docs.
        ## All five docs could be edited with exactly the same tools,
        ## and the user is welcome to adjust their vocab as they see fit.
        ## In fact, there's nothing special about the "title" and
        ## "vocab" Exprs either -- they live in the database
        ## too, addressable thanks to some bootstrapping magic and
        ## hard-coded IDs!
        ##
        ##    ":litom-mahut" -> (Title "vocabFor") (vocabFor "top")
        ##    ":hakot-teret" -> (Title "Title") (vocabFor "top")
        ##

        key*: ID
        children*: seq[Expr]
    Expr* = object
        ## Exprs are the children of Docs.
        ## All Exprs confirm to a specific vocabulary, generally
        ## enforced at read time (e.g. across serialization
        ## boundaries).
        kind*: ID
        val*: string
        children*: seq[Expr]

    Library* = ref object of RootObj
        ## Librarys are key/value stores of Docs.
    MapLibrary* = ref object of Library
        docs: Table[ID, Doc]


proc hash*(id: ID): Hash {.borrow.}
proc `==`*(a, b: ID): bool {.borrow.}
proc `$`*(a: ID): string {.borrow.}
proc len*(a: ID): int {.borrow.}
proc toID*(str: string): ID =
    if unlikely(str.len <= 1):
        raise newException(ValueError, "IDs cannot be blank")
    if unlikely(str[0] != ':'):
        raise newException(ValueError, "ID must start with :. Given " & str)
    return ID(str)

converter toExpr*(doc: Doc): Expr =
    ## Anywhere an Expr is needed, a Doc can be used.
    Expr(kind: doc.key, children: doc.children)

iterator items*(doc: Doc): Expr =
    for expr in doc.children:
        yield expr
iterator items*(doc: Expr): Expr =
    for expr in doc.children:
        yield expr

proc `$`*(exp: Expr): string =
    var res: seq[string]
    proc rec(e: Expr, res: var seq[string]) =
        res.add "(" & $e.kind
        if e.val.len > 0:
            res.add( $ %*e.val)
        for c in e: rec(c, res)
        res.add ")"
    rec(exp, res)
    return res.join " "

# Nicer unwrapping of Optional types
template liftOptional(name, typeA, typeB: untyped): untyped =
    proc `name`*(item: Option[typeA]): Option[typeB] =
        if item.isSome: return some item.get().name
liftOptional(key, Doc, ID)
liftOptional(kind, Expr, ID)
liftOptional(val, Expr, string)

method lookup*(library: Library, id: ID): Option[Doc] {.base.} = none(Doc)
    ## Lookup a doc by ID
method lookup*(library: MapLibrary, id: ID): Option[Doc] =
    if id in library.docs: return some(library.docs[id])
proc lookupDocForExpr*(library: Library, expr: Expr): Option[Doc] =
    ## returns the vocab-defining Doc for a given Expr
    return library.lookup(expr.kind)



method add*(library: Library, docs: varargs[Doc]): Doc {.discardable, base.} =
    ## add an Doc to the library
    raise newException(ObjectAssignmentDefect, "Cannot add a ref to an abstract base library")
method add*(library: MapLibrary, docs: varargs[Doc]): Doc {.discardable.} =
    for exp in docs:
        library.docs[exp.key] = exp
    docs[0]
method contains*(library: Library, key: ID): bool {.base.} = false
method contains*(library: MapLibrary, key: ID): bool =
    key in library.docs

# Built-in library
var builtins* = MapLibrary()
proc newMapLibrary*(): MapLibrary =
    MapLibrary(docs: builtins.docs)

iterator allBuiltins*(): Doc =
    for doc in builtins.docs.values:
        yield doc

proc makeDoc*(id: ID, items: varargs[Expr]): Doc =
    Doc(key: id, children: items.toSeq)
proc newExpr*(doc: Doc, val: string = "", items: varargs[Expr] = []): Expr =
    Expr(kind: doc.key, val: val, children: items.toSeq)
proc newExpr*(key: ID, val: string = "", items: varargs[Expr] = []): Expr =
    Expr(kind: key, val: val, children: items.toSeq)

# Macro for parsing refs and their bodies
proc macroBodyToExpr(body: NimNode): NimNode {.compileTime.} =
    result = newNimNode(nnkCall)
    result.add bindSym"newExpr"
    case body:
    of Ident(strVal: @name):
        result.add body
    of Command[Ident(),
               StrLit()]:
        result.add body[0]
        result.add body[1]
    of Command[Ident(),
               StrLit(),
               StmtList[all @stmts]]:
        result.add body[0]
        result.add body[1]
        for child in stmts:
            result.add child.macroBodyToExpr
    of Call[Ident(),
            StmtList[all @stmts]]:
        result.add body[0]
        result.add newLit("")
        for child in stmts:
            result.add child.macroBodyToExpr
    else:
        raise newException(Defect, "Invalid syntax for ref definition: " & body.treeRepr)
# the real nice macros
macro newDoc*(id: ID): untyped =
    result = nnkCall.newTree(bindsym"makeDoc", id)
macro newDoc*(id: ID, body: untyped): untyped =
    result = nnkCall.newTree(bindsym"makeDoc", id)
    for child in body:
        result.add macroBodyToExpr(child)
macro defBuiltinDoc*(id: ID): untyped =
    quote do:
        builtins.add newDoc(`id`)
macro defBuiltinDoc*(id: ID, body: untyped): untyped =
    quote do:
        builtins.add newDoc(`id`, `body`)

# Time to bootstrap our builtins.
# These first nodes specify IDs manually.
let
    idVocab = ":VSzg5".toId
    idTitle = ":qyQgm".toId
    topDoc* = defBuiltinDoc ":top".toId:
        idTitle "Top scope"
    vocabFor* = defBuiltinDoc idVocab:
        idVocab ":top"
        idTitle "vocab-for"
    title* = defBuiltinDoc idTitle:
        vocabFor ":top"
        idTitle "title"
    vocabHas* = defBuiltinDoc ":EV62N".toId:
        vocabFor ":top"
        title "vocab-has"
        title "vocab-child"

# Basic ops
# IMPORTANT: We never want to call a / b when b is an Expr.
# Said another way, only Docs define vocab, not Exprs.
# The converter above allows the converse and not this,
# which is correct behavior.
iterator `/`*(a: Expr, b: Doc): Expr =
    for child in a:
        if child.kind == b.key:
            yield child
iterator `/`*(a: Expr, kind: ID): Expr =
    for child in a:
        if child.kind == kind:
            yield child
proc has*(a: Expr, b: Doc): bool =
    for expr in a / b: return true
proc hasEqual*(a: Expr, b: Doc, pl: string): bool =
    for expr in a / b:
        if expr.val == pl:
            return true
proc first*(a: Expr, b: Doc): Option[Expr] =
    for expr in a / b: return some expr

# Convenience methods
proc firstTitle*(doc: Doc): Option[string] =
    return (doc.first title).val
iterator allTitles*(doc: Doc): string =
    for expr in doc / title:
        yield expr.val

method searchFor*(library: Library, kind: ID): seq[Doc] {.base.} = discard
method searchFor*(library: MapLibrary, kind: ID): seq[Doc] =
    for doc in library.docs.values:
        for expr in doc / kind:
            result.add doc
            break


proc wouldCauseVocabRegen*(d: Doc): bool =
    return d.has(vocabFor) or d.has(vocabHas)
