import docs
import vocabulary
import strformat
import strutils
import resolution
import sequtils
import fusion/matching
import json

proc reprFull*(expr: Expr): string =
    result = fmt "(:{expr.kind}"
    if expr.val != "":
        result &= " "
        result &= $ %*expr.val
    if expr.children.len > 0:
        result &= " "
        result &= expr.children.mapIt(reprFull it).join(" ")
    result &= ")"

proc reprHumanFriendly*(univ: Library, vocab: Vocabulary, expr: Expr, context = ""): string =
    result = "("
    if Some(@doc) ?= univ.lookup expr.kind:
        block outer:
            for title in doc.allTitles:
                if Some(@doc) ?= vocab.resolveDirectly(title, context):
                    result &= title
                    break outer
            if Some(@title) ?= doc.firstTitle:
                result &= title
                result &= ":"
                result &= expr.kind
            else:
                result &= ":"
                result &= expr.kind

    else:
        result &= ":"
        result &= expr.kind
    if expr.val != "":
        result &= " "
        result &= $ %*expr.val
    if expr.children.len > 0:
        result &= " "
        result &= expr.children.mapIt(reprHumanFriendly(univ, vocab, it, expr.kind)).join(" ")
    result &= ")"

proc reprHumanFriendly*(univ: Library, vocab: Vocabulary, doc: Doc, context = ""): string =
    result = "("
    result &= ":" & doc.key
    if doc.children.len > 0:
        result &= " "
        result &= doc.children.mapIt(univ.reprHumanFriendly(vocab, it)).join(" ")
    result &= ")"
