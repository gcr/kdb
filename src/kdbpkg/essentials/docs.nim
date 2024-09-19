# kdb (c) 2024 Kimberly Wilber.
# kdb is distributed under a license that expressly prohibits military use. See "LICENSE" in this repository for details.

import sequtils
import macros
import options
import tables
import sets
import fusion/matching
import hashes
import json # for stringification
import strutils
import strformat
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
        ##    ":VSzg5" -> (Title "vocabFor") (vocabFor "top")
        ##    ":qyQgm" -> (Title "Title") (vocabFor "top")
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
        ## This class is responsible for serialization/persistence, etc.
    MapLibrary* = ref object of Library
        ## In-memory mapping, typically used for builtins.
        immutable = true
        docs: Table[ID, Doc]


proc hash*(id: ID): Hash {.borrow.}
proc `==`*(a, b: ID): bool {.borrow.}
proc `$`*(a: ID): string {.borrow.}
proc len*(a: ID): int {.borrow.}

proc toID*(str: string): Option[ID] =
    if unlikely(str.len <= 1):
        #raise newException(ValueError, "IDs cannot be blank")
        return none(ID)
    if unlikely(str[0] != ':'):
        #raise newException(ValueError, "ID must start with :. Given " & str)
        return none(ID)
    if unlikely(" " in str):
        return none(ID)
    return some ID(str)

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

## Library methods.
method contains*(library: Library, key: ID): bool {.base.} = false
    ## Returns `true` if `key` exists in the library.
method contains*(library: MapLibrary, key: ID): bool =
    ## Returns `true` if `key` exists in the library.
    key in library.docs
method lookup*(library: Library, id: ID): Option[Doc] {.base.} = none(Doc)
    ## Lookup a doc by ID.
method lookup*(library: MapLibrary, id: ID): Option[Doc] =
    ## Lookup a doc by ID.
    if id in library.docs: return some(library.docs[id])
proc lookupDefinitionOfExpr*(library: Library, expr: Expr): Option[Doc] =
    ## Maps an `expr` to its own definition in the database.
    return library.lookup(expr.kind)
method add*(library: Library, docs: varargs[Doc]): Doc {.discardable, base.} =
    ## Add a Doc to the library.
    raise newException(ObjectAssignmentDefect, "Cannot add a doc to an abstract base library")
method add*(library: MapLibrary, docs: varargs[Doc]): Doc {.discardable.} =
    ## Add a Doc to the library.
    for doc in docs:
        if library.immutable and doc.key in library:
            raise newException(ValueError, fmt"Library is immutable and {doc.key} is already in it")
        library.docs[doc.key] = doc
    docs[0]

## Built-in library.
## External C code probably needs to call NimMain() for setup --
## we define all sorts of builtins right at the top level
## of these nim scripts. This also means that just `importing`
## nim modules has the side effect of adding their builtins
## to your database, but since Doc IDs are assumed to be
## *global and immutable*, that's probably a feature.
var builtins* = MapLibrary()
proc newMapLibrary*(): MapLibrary =
    MapLibrary(docs: builtins.docs, immutable: true)

iterator allBuiltins*(): Doc =
    for doc in builtins.docs.values:
        yield doc

## Macros for making docs and exprs. Sugar for nim code.
proc makeExpr(key: ID, val: string, items: openarray[Expr] = []): Expr =
    Expr(kind: key, val: val, children: items.toSeq)
proc makeExpr(key: ID, val: Doc, items: openarray[Expr] = []): Expr =
    Expr(kind: key, val: $val.key, children: items.toSeq)
proc makeExpr(key: ID, items: openarray[Expr] = []): Expr =
    Expr(kind: key, val: "", children: items.toSeq)
proc makeExpr(doc: Doc, val: string, items: openarray[Expr] = []): Expr =
    Expr(kind: doc.key, val: val, children: items.toSeq)
proc makeExpr(doc: Doc, val: Doc, items: openarray[Expr] = []): Expr =
    # useful for vocab-like items. Imagine makeExpr(vocabHas, topDoc)
    Expr(kind: doc.key, val: $val.key, children: items.toSeq)
proc makeExpr(doc: Doc, items: openarray[Expr] = []): Expr =
    Expr(kind: doc.key, val: "", children: items.toSeq)
proc macroBodyToExpr(body: NimNode): seq[NimNode] {.compileTime.} =
    case (body.kind, body):
    of ({nnkStrLit, nnkCallStrLit}, _):
        # bare string literal: used as payload.
        # when nim binds this to makeExpr, it will do the dirty work here.
        result.add quote do: `body`
    of (nnkIdent, _):
        # bare expression
        result.add quote do: makeExpr(`body`)
    of (nnkStmtList, [all @stmts]):
        # returns multiple results that parent needs to splice in
        for stmt in stmts:
            for res in stmt.macroBodyToExpr:
                result.add res
    of ({nnkCommand, nnkCall, nnkExprEqExpr}, [@ident, @val(it.kind in {nnkStrLit, nnkIdent, nnkSym}), all @rest]):

        # someDoc "abc", vocabFor(topDoc)
        result.add quote do: makeExpr(`ident`, `val`)
        for subexpr in rest:
            for res in subexpr.macroBodyToExpr:
                result[^1].add res
    of ({nnkCommand, nnkCall, nnkExprEqExpr}, [@ident, all @rest]):
        result.add quote do: makeExpr(`ident`)
        for subexpr in rest:
            for res in subexpr.macroBodyToExpr:
                result[^1].add res
    else:
        raise newException(Defect, "Invalid syntax for doc definition:\n" & body.treeRepr)
macro newExpr*(id: untyped, body: varargs[untyped]): untyped =
    ## This is the recommended entry point for creating
    ## expressions. Several syntaxes are supported:
    ##
    ##   newExpr(ID":abcd", "payload", subExpr("payload"), subExpr2("payload"))
    ##   newExpr(someDoc, "payload", subExpr="payload", subExpr2="payload")
    ##   newExpr someDoc("payload"):
    ##     subExpr "payload"
    ##     subExpr2 "payload"
    ##   someDoc("payload", subExpr="payload", subExpr2="payload")
    result = nnkCall.newTree(bindsym"makeExpr", id)
    for child in body:
        for res in child.macroBodyToExpr:
            result.add res
template `()`*(doc: Doc, body: varargs[untyped]): untyped =
  newExpr(doc, body)

proc makeDoc(id: ID, items: varargs[Expr]): Doc =
    Doc(key: id, children: items.toSeq)
macro newDoc*(id: ID, body: varargs[untyped]): untyped =
    ## This is the recommended entry point for creating
    ## new docs, similarly to newExpr.
    ## Several syntaxes are supported:
    ##
    ##   newDoc(ID":abcd", subExpr("payload"), subExpr2("payload"))
    ##   newDoc(ID":abcd", subExpr="payload", subExpr2="payload")
    ##   newDoc ID":abcd":
    ##     subExpr "payload"
    ##     subExpr2 "payload"
    ##   someDoc(subExpr="payload", subExpr2="payload")
    result = nnkCall.newTree(bindsym"makeDoc", id)
    for child in body:
        for res in child.macroBodyToExpr:
            result.add res
macro defBuiltinDoc*(id: ID, body: varargs[untyped]): untyped =
    ## Create a new doc and add it to the builtins.
    quote do:
        assert `id` notin builtins
        assert ($`id`).toID.isSome, "ID must be well-formed"
        builtins.add newDoc(`id`, `body`)

# Time to bootstrap our builtins.
# These first nodes specify IDs manually.
let
    idVocab = ID":VSzg5"
    idTitle = ID":qyQgm"
    idSummary = ID":otNaZ"
    idVocabExplicitOnly = ID":S3Es1"
    topDoc* = defBuiltinDoc(ID":top",
        idTitle="Top scope",
        idsummary="All vocab inherits from this special doc.",
        idVocabExplicitOnly=""
    )
    vocabFor* = defBuiltinDoc(idVocab,
        idVocab=":top",
        idTitle="vocab-for",
        idSummary="Allows this doc to become vocab for the indicated doc."
    )
    # demonstrating more syntax (equivalent to above):
    title* = defBuiltinDoc idTitle:
        vocabFor ":top"
        idTitle "title"
        idSummary "User-friendly doc title. Docs may have multiple titles. Titles are searchable by name."
    vocabHas* = defBuiltinDoc ID":EV62N":
        vocabFor ":top"
        title "vocab-has"
        title("vocab-child")
        idSummary "Allows the indicated doc to become vocab for this doc."
    summary* = defBuiltinDoc idSummary:
        vocabFor ":top"
        title "summary"
        idSummary "One-line summary of vocab entries."
    vocabExplicitOnly* = defBuiltinDoc idVocabExplicitOnly:
        vocabFor ":top"
        title "vocab-explicit-only"
        summary "Prevents implicit structuralization from expanding into this vocab."

# Basic ops
# IMPORTANT: We never want to call a / b when b is an Expr.
# Said another way, only Docs define vocab, not Exprs.
# The converter above allows the converse and not this,
# which is correct behavior.
# Here's how to think of this: in the below definitions, the
# `Doc` type refers to vocab definitions that appear under
# the Expr.
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
    return d.has(vocabFor) or d.has(vocabHas) or d.has(vocabExplicitOnly)
