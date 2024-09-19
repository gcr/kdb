import essentials/[docs, parsing, vocabulary, uuid, repr, vocabMacros]
import libraries/sqliteLibrary
import builtins/textual
import std/[strutils, sequtils, options, streams, times, strformat, terminal, os, tables]
import fusion/matching
import macros
import prettyprinting
import json
import deques

type CLIOptions* = object
  humanReadableOutput* = true
  dryRun* = false

var CLI_REGISTRY: Table[ID, proc(expr: Expr): Expr]

template defCommand*(k, expr, body: untyped): untyped =
  CLI_REGISTRY[k.key] = proc(expr: Expr): Expr = body

proc run*(lib: Library, e: Expr): Expr =
  if e.kind in CLI_REGISTRY:
    return CLI_REGISTRY[e.kind](e)
  else:
    let resultCommand = lib.lookupDefinitionOfExpr(e)
    if Some(@resultExpr) ?= resultCommand:
      if Some(@title) ?= resultExpr.firstTitle:
        raise newException(ValueError, fmt"Command {title} isn't implemented.")
      else:
        raise newException(ValueError, fmt"Command {e.kind} isn't implemented.")
    else:
      raise newException(ValueError, fmt"Command is not a builtin: {e.kind} This is an internal error!")

proc writeToken*(dt: DexprToken, lib: Library, withColor=stdout.isatty(), emphasize=false) =
  proc write(s: string) =
    if withColor and emphasize:
        stdout.styledWrite(fgRed, s)
    elif withColor:
        stdout.styledWrite(styleBright, s)
    else:
        stdout.write(s)
  proc writeSubtle(s: string) =
    if withColor:
        stdout.styledWrite(styleBright, fgWhite, s)
    else:
        stdout.write(s)

  proc writeTitleId() =
    var wrote = false
    if Some(@id) ?= dt.symbol.toID:
      if Some(@doc) ?= lib.lookup(id):
        if Some(@title) ?= doc.firstTitle:
            wrote = true
            if dt.inferredFromImplicitResolution:
                writeSubtle(title)
            else:
                write(title)
            writeSubtle(dt.symbol)
    if not wrote:
        if dt.inferredFromImplicitResolution:
            writeSubtle(dt.symbol)
        else:
            write(dt.symbol)
  case dt.kind:
  of dtkPushNewContextImplicitly:
    writeSubtle " ("
    writeTitleId()
  of dtkPushNewContext:
    write " ("
    writeTitleId()
  of dtkPopContext:
    write ")"
  of dtkPopContextImplicitly:
    writeSubtle ")"
  of dtkBareExp:
    writeSubtle " ("
    writeTitleId()
    writeSubtle ")"
  of dtkStrLit:
    if dt.value.len > 0:
      write " " & ($ %* dt.value)
  of dtkStrLitIncomplete:
    let jsstr = " " & ($ %* dt.value)
    write jsstr.substr(0, jsstr.high-1)
proc writeTokens*(tokens: seq[DexprToken], lib: Library, withColor=stdout.isatty(), emphasize=false) =
    for dt in tokens:
        dt.writeToken(lib, withColor, emphasize)

proc writeError*(message: varargs[string]) =
  if isatty(stderr):
    stderr.styledWrite styleBright, fgRed, "[error] ", fgDefault
    for msg in message: stderr.write msg
    stderr.resetAttributes
    stderr.write("\n")
  else:
    stderr.write "[error] "
    for msg in message: stderr.write msg
    stderr.write("\n")

proc writeParseError*(parser: ParseState, lib: Library) =
  parser.processed.writeTokens(lib)
  parser.failingToken.writeToken(lib, emphasize=true)
  parser.unprocessed.writeTokens(lib)
  stdout.write "\n"
  writeError "Parse error:"
  writeError $parser.locToInputDesc(parser.loc)
  # Which token to highlight?
  #var failingLoc = parser.loc
  #var failingLaterLoc = 0
  #for p in parser.tokens:
  #  if p.loc > failingLoc:
  #    failingLaterLoc = p.loc
  #    break
  ## found it
  #for line in parser.input.splitLines():
  #  if failingLoc > line.len:
  #    stderr.write "   "
  #    stderr.writeLine line
  #  else:
  #    if failingLaterLoc <= 0:
  #      # not sure where to highlight
  #      var before = line.substr(0, failingLoc-1)
  #      var failing = line.substr(failingLoc)
  #      if isatty(stderr):
  #        stderr.styledWriteLine "   " & before, fgRed, failing
  #      else:
  #        stderr.writeLine "   " & before & failing
  #    else:
  #      # we do know where to highlight and stop highlighting
  #      var before = line.substr(0, failingLoc-1)
  #      var failing = line.substr(failingLoc, failingLaterLoc-1)
  #      var after = line.substr(failingLaterLoc)
  #      if isatty(stderr):
  #        stderr.styledWriteLine "   ", before, fgRed, failing, fgDefault, after
  #      else:
  #        stderr.writeLine "   " & before & failing & after
  #    # Now write the nice arrow
  #    if isatty(stderr):
  #      stderr.styledWriteLine fgRed, "   " & repeat("~", max(0, failingLoc)), "^"
  #      stderr.resetAttributes
  #    else:
  #      stderr.writeLine "   " & repeat("~", max(0, failingLoc)), "^"
  #    break
  #  failingLoc -= (line.len + 1) # for the newline
  #  failingLaterLoc -= (line.len + 1)
  writeError parser.message
  echo $parser

proc parseCliOptions*(opts: seq[string], lib: Library, vocab: Vocabulary, rootContext: ID): tuple[options:CLIOptions, exprs: seq[Expr]] =
  result.options = CLIOptions()
  var tokens: seq[string]
  var opts = opts.toDeque()
  var usedStdin = false
  while opts.len > 0:
    let nextTok = opts.popFirst()
    case nextTok:
    of "-n", "--dry-run":
      result.options.dryRun = true
    of "-s", "--str":
      let nextStrLit = opts.popFirst()
      tokens.add($ %*nextStrLit)
    of "-S", "--stdin":
      if usedStdin:
        raise newException(ValueError, "cannot have more than one --stdin")
      tokens.add($ %*stdin.readAll())
      usedStdin = true
    #of "-i", "--from-file":
    #  let filename = opts.popFirst()
    #  var strm = openFileStream(filename)
    #  tokens.add(strm.readAll())
    of "-r", "--human-readable":
      result.options.humanReadableOutput = false
    else:
      tokens.add(nextTok)

  var parser = ParseState(
    vocab: vocab,
  )
  for cliIdx, cliOpt in tokens.pairs():
    parser.inputs.add ParseInput(name: fmt"argument {cliIdx+1}", content: cliOpt)
  parser.parse(rootContext=rootContext)
  if parser.kind == parseOk:
    result.exprs = parser.results
  else:
    parser.writeParseError(lib)
    quit 1


