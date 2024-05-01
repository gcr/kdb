import kdbpkg/essentials/[docs, parsing, vocabulary, uuid, repr]
import kdbpkg/libraries/sqliteLibrary
import std/[strutils, sequtils, options, streams]
import fusion/matching

proc makeDoc(lib: Library, vocab: Vocabulary, exprs: seq[string]): Option[Doc] =
  var doc = makeDoc(uuid())
  var parser = parsing.ParseState(
    input: exprs.join(" "), vocab: vocab)
  parser.parse()
  if parser.kind == parseOk:
    doc.children = parser.results
    return some doc
  else:
    echo exprs.join(" ")
    stderr.write("Couldn't parse\n")
    quit 1

proc p(exprs: seq[string]) =
  var lib = openSqliteLibrary()
  if Some(@doc) ?= makeDoc(lib, lib.getSchema(), exprs):
    echo doc.reprFull
    echo lib.reprHumanFriendly(lib.getSchema(), doc)

proc fromStdin() =
  var lib = openSqliteLibrary()
  let schema = lib.getSchema()
  while true:
    let line = stdin.readLine()
    if Some(@doc) ?= makeDoc(lib, schema, @[line]):
      lib.add doc

proc n(exprs: seq[string]) =
  var lib = openSqliteLibrary()
  if Some(@doc) ?= makeDoc(lib, lib.getSchema(), exprs):
    echo doc.reprFull
    echo lib.reprHumanFriendly(lib.getSchema(), doc)
    lib.add(doc)
    echo "Added."

when isMainModule:
  var lib = openSqliteLibrary()
  import cligen
  cligen.dispatchMulti(
    [p],  [n], [fromStdin]
  )
