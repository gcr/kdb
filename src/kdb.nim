import kdbpkg/essentials/[docs, parsing, vocabulary, uuid, repr, vocabMacros]
import kdbpkg/libraries/sqliteLibrary
import kdbpkg/builtins/textual
import kdbpkg/cliHelpers
import std/[strutils, sequtils, options, streams, times, strformat, terminal, os, tables]
import fusion/matching
import macros
import json
import deques

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


cliNew.defCommand(expr):
  echo "Hi"


when isMainModule:
  var lib = openSqliteLibrary()
  let vocab = lib.getFullVocabulary()
  let cliOps = parseCliOptions(commandLineParams(), lib, vocab, rootContext=cliAPIRequest.key)
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


