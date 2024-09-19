# kdb (c) 2024 Kimberly Wilber.
# kdb is distributed under a license that expressly prohibits military use. See "LICENSE" in this repository for details.

import docs
import macros
import fusion/matching
import sequtils
import sugar
import options
import strformat

## Easier nim macros for defining vocab trees.
##
## Kdb has two major ways of defining vocab:
## - "Top-down," where each doc specifies expressions
##   that it can contain using `vocab-has`
## - "Bottom-up," where each doc specifies the expressions
##   that it can appear under, using `vocab-for`
##
## In terms of structuralization, :a -> (vocab-has :b) and
## :b -> (vocab-for :a) are equivalent. However, in terms
## of engineering intent, these two systems capture different
## shades of meaning:
##
## - Top-down vocab emphasizes to the user that the vocab
##   being defined is assumed to follow a well-defined
##   structure. App-specific vocab will probably follow
##   top-down methods because it's all related.
## - Allowing for bottom-up vocab emphasizes extensibility.
##   If some future package wants to support some feature of,
##   say, textual documents, then it can add vocab-for ":textual"
##   and other clients should handle that gracefully even if they
##   don't know how to render it.
##
## This file lets us quickly define vocabulary trees for nim code.
## Great for writing plugins, processors, filters, etc.
## That way, there's a 1-to-1 relationship between the vocab that
## exists in the builtin docs and a Nim identifier.
##
## TODO: I would _love_ to typecheck this.

proc defBuiltinVocabImpl*(body: NimNode, vocabFor: Option[string]=some ":top", useVocabHas=true, useVocabFor=false): (seq[NimNode], NimNode) {.compileTime.} =
    assert useVocabHas xor useVocabFor, "Need to use EITHER vocabHas (top-down) OR vocabFor (bottom-up), but not both"

    # First, add let statements!
    var resultKeys: seq[NimNode]
    var resultStmts = nnkStmtList.newNimNode()
    for node in body:
        case (node.kind, node):
        of (in {nnkCall, nnkCommand},[@ident, StrLit(strVal: @key), until @kvItems is StmtList(), opt @subVocab]):
            resultKeys.add quote do: `key`
            var subexpr = quote do: newDoc(ID`key`)
            if Some(@vocabForKey) ?= vocabFor:
                subexpr.add quote do: vocabFor `vocabForKey`
            for node in kvItems:
                subexpr.add node
            if Some(@subVocab) ?= subVocab:
                let (childkeys, childStatements) = defBuiltinVocabImpl(
                    subVocab, if useVocabFor: some key else: none(string),
                    useVocabHas=useVocabHas, useVocabFor=useVocabFor
                    )
                for child in childStatements:
                    resultStmts.add child
                if useVocabHas:
                    for doc in childKeys:
                        subexpr.add quote do: vocabHas `doc`
            resultStmts.add quote do:
                doAssert(ID(`key`) notin builtins)
                let `ident`* = builtins.add `subexpr`
        of (nnkIdent, @sym):
            # Reuse some previously-defined doc as vocab child
            resultKeys.add sym
            if Some(@vocabFor) ?= vocabFor:
                raise newException(Defect, fmt"Vocab tree specifies that {sym} is vocab for {vocabFor}. But {sym} is already defined, and adding the expression would require mutating it. Either use top-down vocab (useVocabHas=true) or explicitly define vocab relationships yourself.")
        else:
            raise newException(Defect, "Invalid syntax for topdown vocab definition:\n" & node.treeRepr)
    return (resultKeys, resultStmts)

macro defTopDownBuiltinVocab*(body: untyped): untyped =
    let (_, stmts) = defBuiltinVocabImpl(body, vocabFor=some ":top", useVocabFor=false, useVocabHas=true)
    return stmts


when isMainModule:
    expandMacros:
        defTopDownBuiltinVocab:
            textual ":abc", title="textual", title="yes":
                abc(":def", title="foo")
                def(":def", title="bar"):
                    jkl(":foo", title="jkl")
                    abc
                    npq(":foo2", title="jkl")
                ghi(":def", title="baz")