import refSchema
import npeg
import sugar
import strformat
import options
import fusion/matching
import resolution
import unicode
import strutils
{.experimental: "caseStmtMacros".}

## Parsing happens in two stages:
## First, a stream of values is
## converted into an "unresolved" dexpr,
## which is like a dexpr but each symbol
## hasn't been resolved yet.
## Then, these expressions are "resolved" by:
## 1. turning symbols into fully-qualified IDs,
## 2. consulting the schema to add the
##    necessary structure to the children.
##
## Example:
## parse("doc date "2024" span 'Hello' h1 span 'Test'")
## -> (doc (date "2024"
##         span "hello"
##         h1
##         span "Test")
## After resolution with an appropriate schema,
## this might be structuralized to something like:
## -> (doc (title (date "2024"))
##         (p (span "hello"))
##         (h1 (span "test")))
## which would then become fully-qualified
## with appropriate UUIDs for everything:
## -> ($123 ($456 ($789 "2024"))
##          ($abc ($def "hello")
##          ($ghi ($def "test"))))

type
  DexprTokenKind* = enum
    dtkPushNewContext,
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
  DexprParseState* = object
    strLen*: int
    s*: seq[DexprToken]
proc `$`*(dt: Dexprtoken): string =
  return case dt.kind:
  of dtkPushNewContext: fmt"push({dt.symbol})"
  of dtkPopContext: fmt"pop"
  of dtkPopContextImplicitly: fmt"popi"
  of dtkBareExp: fmt"bare({dt.symbol})"
  of dtkStrLit: fmt"str({dt.value})"
  of dtkStrLitIncomplete: fmt"stri({dt.value})"

proc withSym(tok: DexprToken, id: ID): DexprToken =
  DexprToken(kind:tok.kind, loc:tok.loc, symbol: id, value: tok.value)

let parser = peg("toplevel", ps: DexprParseState):
  S <- *(Blank | ',')
  sexp_open <- S * '(' * S * >reflink * S:
    ps.s.add DexprToken(
      kind: dtkPushNewContext, symbol: $1, loc: @1 )
  mexp_open <- S * >reflink * '(':
    ps.s.add DexprToken(
      kind: dtkPushNewContext, symbol: $1, loc: @1 )
  close <- S * >')' * S:
    ps.s.add DexprToken(kind: dtkPopContext, loc: @1)

  bare_dexpr <- S * >reflink * S:
    ps.s.add DexprToken(kind: dtkBareExp, symbol: $1, loc: @1)

  # String literals!
  strbegin <- >'"':
    ps.s.add DexprToken(kind: dtkStrLit)
  unicodeEscape  <- 'u' * >Xdigit[4]:
    let rune = fromHex[int32]($1)
    ps.s[^1].value.add $Rune(rune)
  escape <- '\\' * (>{ '"', '/', '\\', 'b', 'f', 'n', 'r', 't' } | >unicodeEscape | >rawEscape):
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

  reflink <- +{'0'..'9', 'a'..'z', 'A'..'Z', '-', '_', '$'}
  toplevel <- +(sexp_open * *strlit |
                mexp_open * *strlit |
                bare_dexpr * *strlit |
                close)  * !1

# for testing
proc parseToUnresolvedTokenStream*(input: string): seq[DexprToken] =
  var ps = DexprParseState(strLen: input.len)
  let matches = parser.match(input, ps)
  return ps.s

# A structuralized stream only contains pushContext, popContext, strLit, and pushImplicitContext to indivate schema backtracking.
# All pushes and pops are guaranteed to
# be balanced* and all string
# literals immediately follow pushContexts.
# EXCEPTION: final popImplicitContexts may be omitted,
# to support incremental parsing of incomplete
# dexpressions like "(foo (bar) (baz <caret-here>"
proc structuralize(schema: Schema, input: string): seq[DexprToken] =
  var ps = DexprParseState(strLen: input.len)
  var contexts: seq[ID] = @[""]
  let matches = parser.match(input, ps)
  var unresolvedStream = ps.s
  if not matches.ok:
    raise newException(ValueError, "couldn't even parse this expression, let alone attempt to structuralize it. try again yo")
  while unresolvedStream.len > 0:
    var tok = unresolvedStream[0]
    unresolvedStream.delete(0)
    case tok.kind:
    of dtkPushNewContext:
      # Attempt to resolve symbol
      if Some(@r) ?= schema.resolve(tok.symbol, contexts[^1]):
        result.add tok.withSym r.kind
      else:
        raise newException(ValueError, fmt "{tok.symbol} isn't a field of {contexts[^1]}")
    of dtkPopContextImplicitly:
      raise newException(Defect, "how'd a dtkPopContextImplicitly get in here? the parser should never emit these. Could be trying to structuralize an already-structuralized expression. Haven't thought about it idk, look into this.")
    of dtkPopContext:
      if contexts.len > 0:
         discard contexts.pop()
         result.add tok
      else:
        raise newException(ValueError, fmt "Unbalanced parentheses: ')' without a '('")
    of dtkBareExp:
      if Some(@r) ?= schema.resolve(tok.symbol, contexts[^1]):
        result.add tok.withSym(r.kind)
      else:
        unresolvedStream.insert DexprToken(kind:dtkPopContextImplicitly, loc: tok.loc)
    of dtkStrLit, dtkStrLitIncomplete:
      if result[^1].kind == dtkPushNewContext:
          result.add tok
      else:
        result.add(DexprToken(kind: dtkPopContextImplicitly))
        result.add(DexprToken(kind: dtkPushNewContext, symbol: contexts[^1]))
        result.add tok
  while contexts.len > 1:
    result.add DexprToken(kind: dtkPopContextImplicitly)