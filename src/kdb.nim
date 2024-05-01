# This is just an example to get you started. A typical hybrid package
# uses this file as the main entry point of the application.

import kdbpkg/essentials/[docs, parsing, vocabulary, uuid, repr]
import kdbpkg/libraries/sqliteLibrary
import std/[strutils]

proc p(exprs: varargs[string]) =
  var lib = openSqliteLibrary()
  var vocab = lib.getSchema()
  var doc = makeDoc(uuid())
  var parser = parsing.ParseState(
    input: expr.join(" "), vocab: vocab)
  parser.parse()
  if parser.kind == parseOk:
    doc.children = parser.results
    echo doc.reprFull

  else:
    raise newException(ValueError, "Could not parse: " & $parser)

when isMainModule:
  import cligen
  cligen.dispatchMulti(
    [p], # [add]
  )
