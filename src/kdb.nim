import kdbpkg/essentials/[docs, parsing, vocabulary, uuid, repr]
import kdbpkg/libraries/sqliteLibrary
import std/[strutils, sequtils, options, streams, times, strformat, terminal]
import fusion/matching

proc readExpr(lib: Library, vocab: Vocabulary, exprs: seq[string]): seq[Expr] =
  var parser = ParseState(input: exprs.join(" "), vocab: vocab)
  parser.parse()
  if parser.kind == parseOk:
    return parser.results
  else:
    stderr.styledWriteLine styleBright, fgRed, "[error]", fgDefault, " Parse error:"
    stderr.resetAttributes
    # Which token to highlight?
    var failingLoc = parser.loc
    var failingLaterLoc = 0
    for p in parser.tokens:
      if p.loc > failingLoc:
        failingLaterLoc = p.loc
        break
    # found it
    for line in exprs.join(" ").splitLines():
      if failingLoc > line.len:
        stderr.styledWriteLine "   " & line
      else:
        if failingLaterLoc <= 0:
          # not sure where to highlight
          var before = line.substr(0, failingLoc-1)
          var failing = line.substr(failingLoc)
          stderr.styledWriteLine "   " & before, fgRed, failing
        else:
          var before = line.substr(0, failingLoc-1)
          var failing = line.substr(failingLoc, failingLaterLoc-1)
          var after = line.substr(failingLaterLoc)
          stderr.styledWriteLine "   ", before, fgRed, failing, fgDefault, after
        stderr.styledWriteLine fgRed, "   " & repeat("~", failingLoc), "^"
        stderr.resetAttributes
        break
      failingLoc -= (line.len + 1) # for the newline
      failingLaterLoc -= (line.len + 1)
    stderr.styledWriteLine styleBright, fgRed, "[error] ", fgDefault, parser.message
    quit 1

proc p(exprs: seq[string]) =
  var lib = openSqliteLibrary()
  var vocab = lib.getFullVocabulary()
  var newExprs = readExpr(lib, lib.getFullVocabulary(), exprs)
  for expr in newExprs:
    stdout.styledWriteLine lib.reprHumanFriendly(vocab, expr)

proc fromStdin() =
  var lib = openSqliteLibrary()
  let vocab = lib.getFullVocabulary()
  var strm = newFileStream "/tmp/foo.exprs"
  while true:
    let line = strm.readLine()
    var doc = makeDoc(uuid())
    doc.children = readExpr(lib, lib.getFullVocabulary(), @[line])
    discard lib.add doc

proc bench() =
  var lib = openSqliteLibrary()
  echo fmt"Iterating through all keys..."
  let begin = getTime()
  var count = 0
  #var vocab = lib.getFullVocabulary()
  for doc in lib.allDocs:
    #count += reprHumanFriendly(lib, vocab, doc, "top").len
    #count += doc.reprFull.len
    discard
  echo fmt"Took {getTime()-begin} sec"
  echo fmt"and {count.float / 1024.0 / 1024} MB"

proc n(exprs: seq[string]) =
  var lib = openSqliteLibrary()
  var doc = makeDoc(uuid())
  doc.children = readExpr(lib, lib.getFullVocabulary(), exprs)
  stdout.styledWrite fgWhite, $doc.key, " -> ", fgDefault, "\n"
  for e in doc.children:
    stdout.styledWriteLine lib.reprHumanFriendly(lib.getFullVocabulary(), e)
  lib.add(doc)
  stderr.styledWrite "Added."

when isMainModule:
  var lib = openSqliteLibrary()
  import cligen
  cligen.dispatchMulti(
    [p], [n], [fromStdin], [bench]
  )
