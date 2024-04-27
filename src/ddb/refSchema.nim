import sequtils
import macros
import options
import tables
import sets
import strutils
import strformat
import fusion/matching
import hashes
{.experimental: "caseStmtMacros".}

type
    ID* = string
        ## IDs uniquely identify expressions stored in the database.
    Expression* = object
        ## Expressions are the entry point to your database. Each expression
        ## is analogous to a single record or database "row".
        ## The "columns" are the Annotations that each expression contains.
        ##
        ## Expressions are typically used in one of two ways:
        ## 1. To store data, or
        ## 2. To define the schema that other expressions must conform to.
        ##
        ## Data expressions store richly structured data within their
        ## annotations, much like a JSON document or a s-expression.
        ## Schema expressions give structure to the universe by
        ## specifying the possible nesting structure that annotations
        ## can have.
        ##
        ## For example, suppose we have a universe with the following
        ## five expressions that record a user's vacation:
        ##   "abcd" -> (title "Trip to Barcelona")
        ##             (Journal (Date "2024-01-01"
        ##                        (Text "The plane was late ..."))
        ##                      (Date "2024-01-05"
        ##                        (Visited "Park Güell")
        ##                        (Text "I can't believe...")))
        ##   "efgh" -> (title "Journal") (annotates "")
        ##   "ijkl" -> (title "Date") (annotates "efgh")
        ##   "mnop" -> (title "Text") (annotates "ijkl") (annotates "")
        ##   "qrst" -> (title "Visited") (annotates "ijkl")
        ##
        ## All five expressions live in the database and can be
        ## edited in the same way. The "abcd" node contains the
        ## bulk of the data content, while the "efgh", "ijkl",
        ## "mnop", and "qrst" nodes define the schema that "abcd"
        ## confirms to. Each of these schema expressions
        ## has an "annotates" annotation pointing to either the ID of
        ## some parent schema expression, or "" to denote that the
        ## annotation should appear at the top level of the expression.
        ##
        ## There's no separation between data and schema expressions.
        ## All five of the nodes could be edited with exactly the same tools,
        ## and the user is welcome to adjust their schema as they see fit.
        ## In fact, there's nothing special about the "title" and
        ## "annotates" annotations either -- they live in the database
        ## too, addressable thanks to some bootstrapping magic and
        ## hard-coded IDs!
        ##
        ##    "litom-mahut" -> (Title "Annotates") (Annotates "")
        ##    "hakot-teret" -> (Title "Title") (Annotates "")
        ##

        key*: ID
        children*: seq[Annotation]
    Annotation* = object
        ## Annotations are the children of Expressions.
        ## All Annotations confirm to a specific schema.
        kind*: ID
        val*: string
        children*: seq[Annotation]

    Universe* = ref object of RootObj
      ## Universes are key/value stores of Expressions.
    MapUniverse* = ref object of Universe
      refs: Table[ID, Expression]

converter toAnnotation*(expr: Expression): Annotation =
  ## Anywhere an Annotation is needed, an Expression can be used.
  Annotation(kind: expr.key, children: expr.children)

proc kind*(expr: Expression): ID = expr.key

iterator items*(expr: Expression): Annotation =
  for annot in expr.children:
    yield annot
iterator items*(expr: Annotation): Annotation =
  for annot in expr.children:
    yield annot

# Nicer unwrapping of Optional types
template liftOptional(name, typeA, typeB: untyped): untyped =
  proc `name`*(item: Option[typeA]): Option[typeB] =
    if item.isSome: return some item.get().name
liftOptional(key, Expression, ID)
liftOptional(kind, Annotation, ID)
liftOptional(val, Annotation, string)


method lookup*(universe: Universe, id: ID): Option[Expression] {.base.} = none(Expression)
  ## Lookup a ref by ID
method lookup*(universe: MapUniverse, id: ID): Option[Expression] =
  if id in universe.refs: return some(universe.refs[id])
proc lookupExprFor*(universe: Universe, annot: Annotation): Option[Expression] =
  ## returns the schema-defining Expression for a given annotation
  return universe.lookup(annot.kind)



method add*(universe: Universe, exprs: varargs[Expression]): Expression {.discardable, base.} =
  ## add an expression to the universe
  raise newException(ObjectAssignmentDefect, "Cannot add a ref to an abstract base universe")
method add*(universe: MapUniverse, exprs: varargs[Expression]): Expression {.discardable.} =
  for exp in exprs:
    universe.refs[exp.key] = exp
  exprs[0]
method contains*(universe: Universe, key: ID): bool {.base.} = false
method contains*(universe: MapUniverse, key: ID): bool =
  key in universe.refs

# Built-in universe
var builtins* = MapUniverse()
proc newMapUniverse*(): MapUniverse =
  MapUniverse(refs: builtins.refs)

proc makeExpression*(id: ID, items: varargs[Annotation]): Expression =
    Expression(key:id, children:items.toSeq)
proc newAnnotation*(expr: Expression, val: string="", items: varargs[Annotation]= []): Annotation =
    Annotation(kind: expr.key, val:val, children:items.toSeq)
proc newAnnotation*(key: ID, val: string="", items: varargs[Annotation]= []): Annotation =
    Annotation(kind: key, val:val, children:items.toSeq)

# Macro for parsing refs and their bodies
proc bodyToDexpr(body: NimNode): NimNode {.compileTime.} =
    result = newNimNode(nnkCall)
    result.add bindSym"newAnnotation"
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
            result.add child.bodyToDexpr
    of Call[Ident(),
            StmtList[all @stmts]]:
        result.add body[0]
        result.add newLit("")
        for child in stmts:
            result.add child.bodyToDexpr
    else:
        raise newException(Defect, "Invalid syntax for ref definition: " & body.treeRepr)
# the real nice macros
macro newExpression*(id: string): untyped =
    result = nnkCall.newTree(bindsym"makeExpression", id)
macro newExpression*(id: string, body: untyped): untyped =
    result = nnkCall.newTree(bindsym"makeExpression", id)
    for child in body:
        result.add bodyToDexpr(child)
macro defBuiltinExpression*(id: string): untyped =
  quote do:
    builtins.add newExpression(`id`)
macro defBuiltinExpression*(id: string, body: untyped): untyped =
  quote do:
    builtins.add newExpression(`id`, `body`)

# Time to bootstrap our builtins.
# These first nodes specify IDs manually.
let
    idAnnotates = "litom-mahut"
    idTitle = "hakot-teret"
    annotates* = defBuiltinExpression idAnnotates:
        idAnnotates ""
        idTitle "Annotates"
    title* = defBuiltinExpression idTitle:
        annotates ""
        idTitle "Title"

# Basic ops
# IMPORTANT: We never want to call a / b when b is an Annotation.
# Said another way, only Expressions define schema,
# not Annotations.
# The converter above allows the converse and not this,
# which is correct behavior.
iterator `/`*(a: Annotation, b: Expression): Annotation =
  for child in a:
    if child.kind == b.key:
      yield child
iterator `/`*(a: Annotation, kind: ID): Annotation =
  for child in a:
    if child.kind == kind:
      yield child
proc has*(a: Annotation, b: Expression): bool =
  for annot in a / b: return true
proc has*(a: Annotation, b: Expression, pl: string): bool =
  for annot in a / b:
    if annot.val == pl:
      return true
proc first*(a: Annotation, b: Expression): Annotation =
  for annot in a / b: return annot

# Convenience methods
proc firstTitle*(a: Expression): Option[string] =
  for annot in a / title:
    return some annot.val
proc allTitles*(a: Expression): seq[string] =
  for annot in a / title:
    result.add annot.val

# Finally, schema definitions!

type
  Schema* = Table[string, SchemaRule]
  SchemaRule* = ref object
    expr*: Expression
    children*: HashSet[SchemaRule] ## mapping titles to these objects
proc hash*(ann: Annotation): Hash =
  result = result !& ann.kind.hash !& ann.val.hash !& ann.children.hash
  result = !$result
proc hash*(sr: SchemaRule): Hash =
  result = !$ (result !& sr.expr.hash)
proc `$`*(schema: SchemaRule): string =
  let title = schema.expr.allTitles.toSeq.join ","
  result = fmt"({title}"
  for child in schema.children:
    result &= fmt" {$child}"
  result &= ")"


method search*(universe: Universe, kind: ID): seq[Expression] {.base.} = discard
method search*(universe: MapUniverse, kind: ID): seq[Expression] =
  for expr in universe.refs.values:
    for annot in expr / kind:
      result.add expr
      break

method getSchema*(universe: Universe): Schema {.base.} =
  for expr in universe.search annotates.key:
    var schemarule = result.mgetOrPut(expr.key, SchemaRule())
    schemarule.expr = expr
    for allowedParent in expr / annotates:
      discard result.hasKeyOrPut(allowedParent.val, SchemaRule())
      result[allowedParent.val].children.incl result[expr.key]