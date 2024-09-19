# kdb (c) 2024 Kimberly Wilber.
# kdb is distributed under a license that expressly prohibits military use. See "LICENSE" in this repository for details.

import kdbpkg/essentials/[docs, vocabulary, repr, vocabMacros]
import kdbpkg/libraries/sqliteLibrary
import kdbpkg/builtins/textual
import kdbpkg/cliHelpers
import std/[os, tables]

defTopDownBuiltinVocab:
  cliAPIRequest ":xDSmd",
      summary="top-level API for commandline interface.",
      vocabExplicitOnly:
    cliGet ":TR7n2", title="get",
        summary="Retrieve a doc with a given ID."
    cliNew ":wsPfe", title="new",
        summary="Make a new ID and return it."
    cliPut ":LFcaw", title="put",
        summary="Put a doc and its contents.":
      topDoc
      cliFromFile ":mJ2KT", title="from-file",
          summary="Which file to load the doc from."
    cliVocab ":3hk2s", title="vocab",
        summary="Retrieve all vocabulary for the database."
    cliParse ":88i3C", title="parse",
        summary="Parse and pretty-print a string value.":
      topDoc

  cliAPIResponse ":BuYY4",
      summary="API for responses",
      vocabExplicitOnly:
    cliError ":k9Dnf", title="error",
        summary="Indicates error with input":
      cliParseError ":sxu1S", title="parse-error",
          summary="Indicates parse error"
    cliGetResponse ":iDsfZ", title="get-response",
        summary="Doc with ID and children":
      topDoc
    cliParseResult ":5c18Q", title="parse-result",
        summary="Parse result":
      topDoc
      cliPartialParse ":LwYhN", title="partial-parse",
          summary="Partial parse result"

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


