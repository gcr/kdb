import kdbpkg/essentials/[docs, parsing, vocabulary, uuid, repr]
import kdbpkg/libraries/sqliteLibrary
import std/[strutils, sequtils, options, streams, times, strformat]
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
    stderr.write(parser.message)
    quit 1

proc p(exprs: seq[string]) =
  var lib = openSqliteLibrary()
  if Some(@doc) ?= makeDoc(lib, lib.getFullVocabulary(), exprs):
    echo doc.reprFull
    echo lib.reprHumanFriendly(lib.getFullVocabulary(), doc)

proc fromStdin() =
  var lib = openSqliteLibrary()
  let vocab = lib.getFullVocabulary()
  var strm = newFileStream "/tmp/foo.exprs"
  while true:
    let line = strm.readLine()
    if Some(@doc) ?= makeDoc(lib, vocab, @[line]):
      discard lib.add doc

proc bench() =
  var lib = openSqliteLibrary()
  echo fmt"Iterating through all keys..."
  let begin = getTime()
  var count = 0
  var vocab = lib.getFullVocabulary()
  for doc in lib.allDocs:
    #count += reprHumanFriendly(lib, vocab, doc, "").len
    #count += doc.reprFull.len
    discard
  echo fmt"Took {getTime()-begin} sec"
  echo fmt"and {count.float / 1024.0 / 1024} MB"

proc n(exprs: seq[string]) =
  var lib = openSqliteLibrary()
  if Some(@doc) ?= makeDoc(lib, lib.getFullVocabulary(), exprs):
    echo doc.reprFull
    echo lib.reprHumanFriendly(lib.getFullVocabulary(), doc)
    lib.add(doc)
    echo "Added."

when isMainModule:
  var lib = openSqliteLibrary()
  import cligen
  cligen.dispatchMulti(
    [p],  [n], [fromStdin], [bench]
  )
