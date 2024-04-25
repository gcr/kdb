import sequtils
import macros
import options
import tables

import fusion/matching
{.experimental: "caseStmtMacros".}

## The grammar is:
##  REF ::= id SEXP*
##
##  SEXP ::= REFLINK
##         | REFLINK payload
##         | REFLINK SEXP*

type
    ID* = string
    Dexpr* = ref object of RootObj
        ## D-expressions are trees of structured data that
        ## conform to a universal schema.
        reflink*: ID
        payload*: string
        children*: seq[Dexpr]
    Ref* = ref object of RootObj
        ## Refs are stored inside the database.
        ## Note the similarity between Ref and Dexpr --
        ## essentially, a Ref is a top-level Dexpr.
        ## They don't have a payload because we want to
        ## discourage "schemaless" content inside the ref--
        ## for example, if a Ref had text, it should be inside
        ## a (doc) dexpr or (binary) or something.
        reflink*: ID
        children*: seq[Dexpr]

    Universe* = ref object of RootObj
      ## Universes are key/value stores of Refs.
    MapUniverse* = ref object of Universe
      refs: Table[ID, Ref]

converter toDexpr*(rref: Ref): Dexpr =
  ## Anywhere a Dexpr is needed, a Ref can be used.
  Dexpr(reflink: rref.reflink, children: rref.children)

proc reflink*(rref: Option[Ref]): Option[ID] =
  if Some(@rr) ?= rref: return some rr.reflink
proc reflink*(odxp: Option[Dexpr]): Option[ID] =
  if Some(@dxp) ?= odxp: return some dxp.reflink

method lookup*(universe: Universe, id: ID): Option[Ref] {.base.} = none(Ref)
  ## Lookup a ref by ID
method lookup*(universe: MapUniverse, id: ID): Option[Ref] =
  if id in universe.refs: return some(universe.refs[id])
proc lookupSchema*(universe: Universe, dxp: Dexpr): Option[Ref] =
  return universe.lookup(dxp.reflink)

method add*(universe: Universe, rref: varargs[Ref]): Ref {.discardable, base.} =
  raise newException(ObjectAssignmentDefect, "Cannot add a ref to an abstract base universe")
method add*(universe: MapUniverse, rrefs: varargs[Ref]): Ref {.discardable.} =
  for r in rrefs:
    universe.refs[r.reflink] = r
  rrefs[0]
method contains*(universe: Universe, rref: Ref): bool {.base.} = false
method contains*(universe: MapUniverse, rref: Ref): bool =
  rref.reflink in universe.refs

method fieldsFor*(universe: Universe, context: ID): seq[Ref] {.base.} = discard

# Built-in universe
var builtins* = MapUniverse()

proc makeRef*(id: ID, children: varargs[Dexpr]): Ref =
    Ref(reflink:id, children:children.toSeq)
proc newDexpr*(rref: Ref, payload: string="", children: varargs[Dexpr]= []): Dexpr =
    Dexpr(reflink: rref.reflink, payload:payload, children:children.toSeq)
proc newDexpr*(reflink: ID, payload: string="", children: varargs[Dexpr]= []): Dexpr =
    Dexpr(reflink: reflink, payload:payload, children:children.toSeq)

# Macro for parsing refs and their bodies
proc bodyToDexpr*(body: NimNode): NimNode {.compileTime.} =
    result = newNimNode(nnkCall)
    result.add bindSym"newDexpr"
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

proc newMapUniverse*(): MapUniverse =
  MapUniverse(refs: builtins.refs)


macro newRef*(id: string): untyped =
    result = nnkCall.newTree(bindsym"makeRef", id)
macro newRef*(id: string, body: untyped): untyped =
    result = nnkCall.newTree(bindsym"makeRef", id)
    for child in body:
        result.add bodyToDexpr(child)
macro defBuiltinRef*(id: string): untyped =
  quote do:
    builtins.add newRef(`id`)
macro defBuiltinRef*(id: string, body: untyped): untyped =
  quote do:
    builtins.add newRef(`id`, `body`)

# these first nodes need to specify IDs manually for bootstrapping
let
    idField = "litom-mahut"
    idTitle = "hakot-teret"
    📝Field* = defBuiltinRef idField:
        idField
        idTitle "Field"
    📝Title* = defBuiltinRef idTitle:
        📝Field
        idTitle "Title"

# For validation
proc payload*(a: Option[Dexpr]): Option[string] =
  if Some(@dx) ?= a: return some dx.payload
proc payloads*(a: seq[Dexpr]): seq[string] =
  return a.mapIt(it.payload)
iterator items*(a: Dexpr): Dexpr =
  for c in a.children: yield c
iterator `/`*(a: Dexpr, b: Ref): Dexpr =
  for child in a:
    if child.reflink == b.reflink:
      yield child
iterator `/^`*(a: Dexpr, b: Ref): string =
  for child in a / b:
    yield child.payload
proc `//`*(a: Dexpr, b: Ref): Option[Dexpr] =
  for child in a / b: return some(child)
proc `//`*(a: Option[Dexpr], b: Ref): Option[Dexpr] =
  if Some(@av) ?= a: return av // b
proc has*(a: Dexpr, b: Ref): bool =
  return (a // b).isSome
proc has*(a: Dexpr, b: Ref, pl: string): bool =
  for field in a / b:
    if field.payload == pl:
      return true
# Convenience methods
proc title*(a: Ref): string =
  return (a // 📝Title).payload.get("")
proc titles*(a: Ref): seq[string] =
  return (a / 📝Title).toSeq.payloads

method fieldsFor*(muniverse: MapUniverse, context: ID): seq[Ref] =
  for rref in muniverse.refs.values:
    for fieldSpecs in rref / 📝Field:
      if fieldSpecs.payload == context:
        result.add rref
proc fieldsFor*(universe: Universe, rref: Ref): seq[Ref] =
  fieldsFor(universe, rref.reflink)

