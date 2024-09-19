# kdb (c) 2024 Kimberly Wilber.
# kdb is distributed under a license that expressly prohibits military use. See "LICENSE" in this repository for details.

import docs
import vocabulary
import strutils
import resolution
import fusion/matching
import json
import re

proc canParse(title: string): bool =
    ## Returns whether the title is safe
    ## to output in human-readable format.
    ## For instance, titles with spaces
    ## aren't safe.
    ## This should be conservative, but
    ## do try to keep it in sync with parsing.nim
    return match(title, re"^[0-9a-zA-Z_:!?/~%$@-]*$")

proc reprTitleOrId*(univ: Library, vocab: Vocabulary, context: ID, expr: Expr): string =
    if Some(@doc) ?= univ.lookup expr.kind:
        # Do any titles resolve directly?
        # Just return them if so
        for title in doc.allTitles:
            if title.canParse:
                if Some(@doc) ?= vocab.resolveDirectly(title, context):
                    return title
        # Nothing resolves directly...but we
        # can still make things nicer for the user.
        # Try again, emitting "title:id"
        # for the first title that can parse
        for title in doc.allTitles:
            if title.canParse:
                return title & $expr.kind
    # not a chance: just return ":key"
    return $expr.kind

proc reprHumanFriendly*(univ: Library, vocab: Vocabulary, expr: Expr, context = ID":top", pieces: var seq[string]) =
    pieces.add "("
    pieces.add reprTitleOrId(univ, vocab, context, expr)
    if expr.val != "":
        pieces.add " "
        pieces.add($ %*expr.val)
    if expr.children.len > 0:
        for child in expr:
            pieces.add " "
            reprHumanFriendly(univ, vocab, child, expr.kind, pieces)
    pieces.add ")"

proc reprHumanFriendly*(univ: Library, vocab: Vocabulary, expr: Expr, context = ID":top"): string =
    var pieces: seq[string]
    reprHumanFriendly(univ, vocab, expr, context, pieces)
    return pieces.join()

