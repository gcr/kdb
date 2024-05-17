import kdbpkg/essentials/[docs, parsing, vocabulary, uuid, repr]
import kdbpkg/libraries/sqliteLibrary
import kdbpkg/builtins/textual
import std/[strutils, sequtils, options, streams, times, strformat, terminal]
import fusion/matching
import macros

proc writeError(message: varargs[string]) =
  if isatty(stderr):
    stderr.styledWrite styleBright, fgRed, "[error] ", fgDefault
    for msg in message: stderr.write msg
    stderr.resetAttributes
    stderr.write("\n")
  else:
    stderr.write "[error] "
    for msg in message: stderr.write msg
    stderr.write("\n")

proc writeParseError(parser: ParseState) =
  writeError "Parse error:"
  # Which token to highlight?
  var failingLoc = parser.loc
  var failingLaterLoc = 0
  for p in parser.tokens:
    if p.loc > failingLoc:
      failingLaterLoc = p.loc
      break
  # found it
  for line in parser.input.splitLines():
    if failingLoc > line.len:
      stderr.write "   "
      stderr.writeLine line
    else:
      if failingLaterLoc <= 0:
        # not sure where to highlight
        var before = line.substr(0, failingLoc-1)
        var failing = line.substr(failingLoc)
        if isatty(stderr):
          stderr.styledWriteLine "   " & before, fgRed, failing
        else:
          stderr.writeLine "   " & before & failing
      else:
        # we do know where to highlight and stop highlighting
        var before = line.substr(0, failingLoc-1)
        var failing = line.substr(failingLoc, failingLaterLoc-1)
        var after = line.substr(failingLaterLoc)
        if isatty(stderr):
          stderr.styledWriteLine "   ", before, fgRed, failing, fgDefault, after
        else:
          stderr.writeLine "   " & before & failing & after
      # Now write the nice arrow
      if isatty(stderr):
        stderr.styledWriteLine fgRed, "   " & repeat("~", max(0, failingLoc)), "^"
        stderr.resetAttributes
      else:
        stderr.writeLine "   " & repeat("~", max(0, failingLoc)), "^"
      break
    failingLoc -= (line.len + 1) # for the newline
    failingLaterLoc -= (line.len + 1)
  writeError parser.message

proc write(doc: Doc, lib: Library, vocab: Vocabulary) =
  if isatty(stdout):
    stdout.styledWriteLine fgWhite, $doc.key, " -> ", fgDefault
  else:
    stdout.writeLine $doc.key, " -> "

  for e in doc.children:
    stdout.writeLine lib.reprHumanFriendly(vocab, e)


proc readExpr(lib: Library, vocab: Vocabulary, exprs: seq[string]): seq[Expr] =
  var parser = ParseState(input: exprs.join(" "), vocab: vocab)
  parser.parse()
  if parser.kind == parseOk:
    return parser.results
  else:
    parser.writeParseError()
    quit 1

proc p(exprs: seq[string]) =
  var lib = openSqliteLibrary()
  var vocab = lib.getFullVocabulary()
  var newExprs = readExpr(lib, lib.getFullVocabulary(), exprs)
  for expr in newExprs:
    stdout.writeLine lib.reprHumanFriendly(vocab, expr)

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
  lib.add(doc)
  #write(doc, lib, lib.getFullVocabulary())
  stdout.write(doc.key)

when isMainModule:
  var lib = openSqliteLibrary()
  import cligen
  cligen.dispatchMulti(
    [p], [n], [fromStdin], [bench]
  )

