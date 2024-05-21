import docs
import vocabulary
import npeg
import strformat
import options
import fusion/matching
import resolution
import unicode
import strutils
import seqUtils
import deques
import json
{.experimental: "caseStmtMacros".}

## Parsing happens in two stages:
## First, a stream of values is
## converted into an "unresolved" dexpr,
## which is like a dexpr but each symbol
## hasn't been resolved yet.
## Then, these Docs are "resolved" by:
## 1. turning symbols into fully-qualified IDs,
## 2. consulting the vocab to add the
##    necessary structure to the children.
##
## Example:
## parse("doc date "2024" span 'Hello' h1 span 'Test'")
## -> (doc (date "2024"
##         span "hello"
##         h1
##         span "Test")
## After resolution with an appropriate vocab,
## this might be structuralized to something like:
## -> (doc (title (date "2024"))
##         (p (span "hello"))
##         (h1 (span "test")))
## which would then become fully-qualified
## with appropriate UUIDs for everything:
## -> (:123 (:456 (:789 "2024"))
##          (:abc (:def "hello")
##          (:ghi (:def "test"))))

type
  DexprTokenKind* = enum
    dtkPushNewContext,
    dtkPushNewContextImplicitly,
    dtkPopContext,
    dtkPopContextImplicitly,
    dtkBareExp,
    dtkStrLit,
    dtkStrLitIncomplete
  DexprToken* = object
    kind*: DexprTokenKind
    loc*: int
    symbol*: string
    value*: string
    rawStrLitSkipLen*: int
  TokenParseState* = object
    strLen*: int
    s*: seq[DexprToken]
proc `$`*(dt: DexprToken): string =
  return case dt.kind:
  of dtkPushNewContextImplicitly: fmt"pushi({dt.symbol})"
  of dtkPushNewContext: fmt"push({dt.symbol})"
  of dtkPopContext: fmt"pop"
  of dtkPopContextImplicitly: fmt"popi"
  of dtkBareExp: fmt"bare({dt.symbol})"
  of dtkStrLit: fmt"str({dt.value})"
  of dtkStrLitIncomplete: fmt"stri({dt.value})"

proc withSym(tok: DexprToken, id: string): DexprToken =
  DexprToken(kind: tok.kind, loc: tok.loc, symbol: id, value: tok.value)
proc withKind(tok: DexprToken, kind: DexprTokenKind): DexprToken =
  DexprToken(kind: kind, loc: tok.loc, symbol: tok.symbol, value: tok.value)


type
  ParseResultStatus* = enum
    parseOk, ## Parse is valid, but incomplete so far.
    parseFail
  ParseState* = object
    input*: string
    vocab*: Vocabulary
    tokens*: seq[DexprToken]
    results*: seq[Expr]
    kind*: ParseResultStatus
    preStructuralize*: seq[DexprToken]
    incompleteContexts*: seq[tuple[key: ID, isExplicit: bool]]
    # for failures:
    message*: string
    loc*: int
    processed*: seq[DexprToken]
    unprocessed*: seq[DexprToken]
    failingToken*: DexprToken

proc setFail(ps: var ParseState, loc: int, message: string, processed: seq[DexprToken], unprocessed: seq[DexprToken], failingToken: DexprToken) =
  ps.kind = parseFail
  ps.loc = loc
  ps.message = message
  ps.processed = processed
  ps.unprocessed = unprocessed
  ps.failingToken = failingToken


let tokenPegParser = peg("toplevel", ps: TokenParseState):
  S <- *(Blank | ',' | '\n')
  sexp_open <- S * '(' * S * >reflink * S:
    ps.s.add DexprToken(
      kind: dtkPushNewContext, symbol: $1, loc: @1)
  mexp_open <- S * >reflink * '(':
    ps.s.add DexprToken(
      kind: dtkPushNewContext, symbol: $1, loc: @1)
  close <- S * >')' * S:
    ps.s.add DexprToken(kind: dtkPopContext, loc: @1)

  bare_dexpr <- S * >reflink * S:
    ps.s.add DexprToken(kind: dtkBareExp, symbol: $1, loc: @1)

  # String literals!
  strbegin <- >'"':
    ps.s.add DexprToken(kind: dtkStrLit, loc: @1)
  unicodeEscape <- 'u' * >Xdigit[4]:
    let rune = fromHex[int32]($1)
    ps.s[^1].value.add $Rune(rune)
  escape <- '\\' * ( > {'"', '/', '\\', 'b', 'f', 'n', 'r', 't'} |
      >unicodeEscape | >rawEscape):
    case $1:
    #of "": discard # unicodeEscape already got it
    of "\"": ps.s[^1].value.add "\""
    of "/": ps.s[^1].value.add "/"
    of "\\": ps.s[^1].value.add "\\"
    of "b": ps.s[^1].value.add "\b"
    of "f": ps.s[^1].value.add "\f"
    of "n": ps.s[^1].value.add "\n"
    of "r": ps.s[^1].value.add "\r"
    of "t": ps.s[^1].value.add "\t"
  unescapedRun <- >+({'\x20'..'\xff'} - {'"'} - {'\\'}):
    ps.s[^1].value.add $1
  stringBody <- *(unescapedRun | escape)
  strIncomplete <- 0:
    ps.s[^1].kind = dtkStrLitIncomplete
  strlit <- S * strbegin * ?stringBody * ('"'|strIncomplete) * S

  # Raw binary escape! "\R 5 xxxxx" -> "xxxxx"
  # Allows any character
  rawEscapeStart <- "R " * >+Digit * " ":
    let len = min(($1).parseInt, ps.strLen-si_NP-1)
    si_NP.inc len # magic: expand the size of this match
    ps.s[^1].rawStrLitSkipLen = ($1).len + 3
    # skip over the R and the two spaces
  rawEscape <- >rawEscapeStart:
    ps.s[^1].value.add ($1).substr ps.s[^1].rawStrLitSkipLen

  # Identifiers
  # Note: Keep this is sync with repr/titleCanParse
  reflink <- +{'0'..'9', 'a'..'z', 'A'..'Z', '-', '_', ':', '!', '?', '/', '~', '%', '$', '@'}
  toplevel <- +(sexp_open * *strlit |
                mexp_open * *strlit |
                bare_dexpr * *strlit |
                close) * !1

# for testing
proc processTokenStream*(p: var ParseState) =
  var tps = TokenParseState(strLen: p.input.len)
  if p.input.len > 0:
    let matches = tokenPegParser.match(p.input, tps)
    p.tokens = tps.s
    if not matches.ok:
      p.kind = parseFail
      p.loc = matches.matchMax-1
      p.message = "Failed to parse"

# A structuralized stream only contains pushContext, popContext, strLit, and pushImplicitContext to indivate vocab backtracking.
# All pushes and pops are guaranteed to
# be balanced* and all string
# literals immediately follow pushContexts.
# EXCEPTION: final popImplicitContexts may be omitted,
# to support incremental parsing of incomplete
# dDocs like "(foo (bar) (baz <caret-here>"
proc structuralize*(ps: var ParseState, context = ID":top") =
  var contexts: seq[tuple[key: ID, isExplicit: bool]] = @[(context, true)]
  var resolved: Deque[DexprToken]
  var unresolvedstream = ps.tokens.toDeque
  if ps.kind != parseOk:
    return
  while unresolvedStream.len > 0:
    var tok = unresolvedStream.popFirst()

    template fail(message: string): untyped =
      ps.setFail(tok.loc, fmt(message), processed=resolved.toSeq, unprocessed=unresolvedstream.toSeq, failingToken=tok)
      return

    when false:
      echo "__TRACE resolved: ", $resolved
      echo "__      contexts: ", $contexts
      echo "__     cur token: ", $tok, " @ ", $tok.loc
      echo "__    unresolved: ", $unresolvedStream

    case tok.kind:

    of dtkPushNewContext, dtkPushNewContextImplicitly:
      # Descend deeper into the structure!
      # Attempt to resolve symbol
      if Some(@path) ?= ps.vocab.resolve(tok.symbol, contexts[^1].key):
        for newCtx in path[0..^2]:
          contexts.add (key: newCtx.key, isExplicit: false)
          resolved.addLast DexprToken(kind: dtkPushNewContextImplicitly,
              loc: tok.loc, symbol: $newCtx.key)
        resolved.addLast tok.withSym $path[^1].key
        contexts.add (key: path[^1].key, isExplicit: tok.kind == dtkPushNewContext)
      else:
        # Backtrack
        # yes, this is necessary for explicit context pushes too! :-)
        unresolvedStream.addFirst tok
        unresolvedStream.addFirst DexprToken(kind: dtkPopContextImplicitly, loc: tok.loc)
        #fail "{tok.symbol} isn't a field of {contexts[^1].key}"
    of dtkPopContext:
      # Explicitly ascend from the structure. Only emitted
      # with an explicit ')', so this should jump out of all
      # of our implicit contexts too.
      let (_, wasExplicit) = contexts.pop()
      if contexts.len == 0:
        fail "Unbalanced parentheses: ')' without a '('"
      if wasExplicit:
        resolved.addLast tok
      else:
        resolved.addLast tok.withKind(dtkPopContextImplicitly)
        # Backtracking until the last explicit context
        unresolvedStream.addFirst tok

    of dtkPopContextImplicitly:
      if len(contexts) == 1:
        if len(unresolvedStream) > 0:
          # might have a better error message for ye
          tok = unresolvedstream.popFirst() # for better error messages
          fail "Couldn't unambiguously resolve symbol {tok.symbol}"
        fail "Tried to break out of the only context."
      let (ctxId, wasExplicit) = contexts.pop()
      if wasExplicit:
        if len(unresolvedStream) > 0:
          # might have a better error message for ye
          tok = unresolvedstream.popFirst() # for better error messages
          fail "Couldn't unambiguously resolve symbol {tok.symbol} inside {ctxId}"
        fail "Tried to implicitly pop out of an explicit context."
      resolved.addLast tok

    of dtkBareExp:
      if Some(@path) ?= ps.vocab.resolve(tok.symbol, contexts[^1].key):
        for newCtx in path[0..^2]:
          contexts.add (key: newCtx.key, isExplicit: false)
          resolved.addLast DexprToken(kind: dtkPushNewContextImplicitly,
              loc: tok.loc, symbol: $newCtx.key)
        contexts.add (key: path[^1].key, isExplicit: false)
        resolved.addLast DexprToken(kind: dtkPushNewContextImplicitly,
            loc: tok.loc, symbol: $path[^1].key)
      else:
        # Backtrack
        unresolvedStream.addFirst tok
        unresolvedStream.addFirst DexprToken(kind: dtkPopContextImplicitly, loc: tok.loc)

    of dtkStrLit, dtkStrLitIncomplete:
      if resolved[^1].kind notin {dtkPushNewContext, dtkPushNewContextImplicitly}:
        resolved.addLast(DexprToken(kind: dtkPopContextImplicitly, loc: tok.loc))
        resolved.addLast(DexprToken(kind: dtkPushNewContextImplicitly,
            symbol: $contexts[^1].key, loc: tok.loc))
      if tok.kind == dtkStrLitIncomplete:
        fail "Unterminated string literal."
      resolved.addLast tok

  ps.incompleteContexts = contexts
  while contexts.len > 1:
    resolved.addLast DexprToken(kind: dtkPopContextImplicitly)
    discard contexts.pop
  ps.kind = parseOk
  ps.preStructuralize = ps.tokens
  ps.tokens = resolved.toSeq

proc processExprs*(ps: var ParseState) =
  ## convert a tokenStream into Expressions
  ## and save them in ParseState
  var stack: seq[Expr] = @[Expr()]
  var processed: seq[DexprToken]
  var unresolvedStream = ps.tokens.toDeque
  if ps.kind == parseOk:
    while unresolvedStream.len > 0:
      var tok = unresolvedStream.popFirst()
      template fail(message: string): untyped =
        ps.setFail(tok.loc, fmt(message), processed = processed, unprocessed = unresolvedStream.toSeq, failingToken=tok)
        return
      case tok:
      of PushNewContext(), PushNewContextImplicitly():
        let symbol: TitleId = tok.symbol
        if $symbol.id == "":
          fail "This symbol must start with a colon: {tok.symbol}"
        stack.add Expr(kind: symbol.id)
      of BareExp():
        fail "Internal error: Bare expr found"
      of PopContext(), PopContextImplicitly():
        stack[^2].children.add stack.pop
      of StrLit(), StrLitIncomplete():
        stack[^1].val = tok.value
      processed.add tok
    ps.results = stack[0].children


proc parse*(ps: var ParseState, rootContext = ID":top") =
  ps.processTokenStream
  if ps.kind == parseOk:
    if ps.vocab.len > 0:
      ps.structuralize(rootContext)
  if ps.kind == parseOk:
    ps.processExprs
