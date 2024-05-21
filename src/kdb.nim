import kdbpkg/essentials/[docs, parsing, vocabulary, uuid, repr, vocabMacros]
import kdbpkg/libraries/sqliteLibrary
import kdbpkg/builtins/textual
import std/[strutils, sequtils, options, streams, times, strformat, terminal, os, tables]
import fusion/matching
import macros
import json
import deques

{.experimental:"callOperator".}

defTopDownBuiltinVocab:
  cliAPIRequest ":xDSmd", summary="top-level API for commandline interface":
    cliGet ":TR7n2", title="get", summary="Get a doc"
    cliNew ":wsPfe", title="new", summary="Make a new ID and return it"
    cliPut ":LFcaw", title="put", summary="Put a doc":
      cliFromFile ":mJ2KT", title="from-file", summary="Load from file"
    cliVocab ":3hk2s", title="vocab", summary="Retrieve vocab / schema for a context"
    cliParse ":88i3C", title="parse", summary="Parse and pretty-print a string"

  cliAPIResponse ":BuYY4", summary="API for responses":
    cliError ":k9Dnf", title="error", summary="Indicates error with input":
      cliParseError ":sxu1S", title="parse-error", summary="Indicates parse error"
    cliGetResponse ":iDsfZ", title="get-response", summary="Doc with ID and children":
      topDoc
    cliParseResult ":5c18Q", title="parse-result", summary="Parse result":
      cliPartialParse ":LwYhN", title="partial-parse", summary="Partial parse result"
      topDoc

type CLIOptions = object
  humanReadableOutput = true
  dryRun = false

var CLI_REGISTRY: Table[ID, proc(expr: Expr): Expr]
template defCommand(k, expr, body: untyped): untyped =
  CLI_REGISTRY[k.key] = proc(expr: Expr): Expr = body
proc run(lib: Library, e: Expr): Expr =
  if e.kind in CLI_REGISTRY:
    return CLI_REGISTRY[e.kind](e)
  else:
    let resultCommand = lib.lookupDocForExpr(e)
    if Some(@resultExpr) ?= resultCommand:
      if Some(@title) ?= resultExpr.firstTitle:
        return cliError.newExpr(fmt"Command {title} isn't implemented.")
      else:
        return cliError.newExpr(fmt"Command {e.kind} isn't implemented.")
    else:
      return cliError.newExpr(fmt"Command is not a builtin: {e.kind} This is an internal error!")

cliNew.defCommand(expr):
  echo "Hi"


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

proc cliOptionsToExpr(opts: seq[string], vocab: Vocabulary): tuple[options:CLIOptions, exprs: seq[Expr]] =
  result.options = CLIOptions()
  var tokens: seq[string]
  var opts = opts.toDeque()
  var usedStdin = false
  while opts.len > 0:
    let nextTok = opts.popFirst()
    case nextTok.toLowerAscii():
    of "-n", "--dry-run":
      result.options.dryRun = true
    of "-s", "--str":
      let nextStrLit = opts.popFirst()
      tokens.add($ %*nextStrLit)
    of "-S", "--stdin":
      if usedStdin:
        raise newException(ValueError, "cannot have more than one --stdin")
      tokens.add(stdin.readAll())
      usedStdin = true
    of "-i", "--from-file":
      let filename = opts.popFirst()
      var strm = openFileStream(filename)
      tokens.add(strm.readAll())
    of "-r", "--human-readable":
      result.options.humanReadableOutput = false
    else:
      tokens.add(nextTok)
  var parser = ParseState(input: tokens.join(" "), vocab: vocab)
  parser.parse(rootContext=cliAPIRequest.key)
  if parser.kind == parseOk:
    result.exprs = parser.results
  else:
    parser.writeParseError()
    quit 1



macro defstuff(doc: Doc, a: varargs[untyped]): untyped =
  echo a.treeRepr
  return quote do: "..."
template `()`(doc: Doc, body: varargs[untyped]): untyped =
  defstuff(doc, payload, body)
let x= cliNew "o", "b", c="yo"
echo x
#echo cliNew("Test", cliError("uh oh"))



when isMainModule:
  var lib = openSqliteLibrary()
  let vocab = lib.getFullVocabulary()
  let cliOps = cliOptionsToExpr(commandLineParams(), vocab)
  proc write(e: Expr) =
    if cliOps.options.humanReadableOutput:
      stdout.writeLine(lib.reprHumanFriendly(vocab, e, context=cliAPIRequest.key))
    else:
      stdout.writeLine($e)
  if cliOps.options.dryRun:
    for e in cliOps.exprs:
      write(e)
  else:
    for e in cliOps.exprs:
      let resultExpr = run(lib, e)
      write(resultExpr)


