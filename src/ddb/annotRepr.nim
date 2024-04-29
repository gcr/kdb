import refSchema
import strformat
import strutils
import resolution
import sequtils
import fusion/matching
import json

proc reprFull*(ann: Annotation): string =
    result = fmt "(:{ann.kind}"
    if ann.val != "":
        result &= " "
        result &= $ %*ann.val
    if ann.children.len > 0:
        result &= ann.children.mapIt(reprFull it).join(" ")
    result &= ")"

proc reprHumanFriendly*(univ: Universe, schema: Schema, ann: Annotation, context = ""): string =
    result = "("
    if Some(@expr) ?= univ.lookup ann.kind:
        block outer:
            for title in expr.allTitles:
                if Some(@expr) ?= schema.resolveDirectly(title, context):
                    result &= title
                    break outer
            if Some(@title) ?= expr.firstTitle:
                result &= title
                result &= ":"
                result &= ann.kind
            else:
                result &= ":"
                result &= ann.kind

    else:
        result &= ":"
        result &= ann.kind
    if ann.val != "":
        result &= " "
        result &= $ %*ann.val
    if ann.children.len > 0:
        result &= " "
        result &= ann.children.mapIt(reprHumanFriendly(univ, schema, it, ann.kind)).join(" ")
    result &= ")"