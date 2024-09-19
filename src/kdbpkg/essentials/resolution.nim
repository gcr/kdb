# kdb (c) 2024 Kimberly Wilber.
# kdb is distributed under a license that expressly prohibits military use. See "LICENSE" in this repository for details.

import vocabulary
import docs
import options
import strutils
import fusion/matching
import tables
import hashes
import sets
{.experimental: "caseStmtMacros".}

## "Resolution" is the process by which
## unstructuralized expressions like
##     textual span "Hello" bold "World"
## is structuralized into
##   (textual:cjYuE (para:3BEuP
##                      (span:G2ENB "Hello")
##                      (span:G2ENB (bold:uwPko "World"))))
## with the help of the current database's vocabulary.
## This goes hand-in-hand with parsing.
##
## Structuralization typically happens at serialization
## boundaries and is generally intended only for user-written
## expressions. It's purely a data-entry / keystroke saving
## measure.
##
## Structuralization is unambiguous, but may depend on
## the current database's user-defined vocabulary.
## We don't tolerate ambiguous structuralizations.
## Because underspecified expressions can cause
## structuralization to fail, other programs that
## write into the database should almost always
## use explicit IDs. To aid human-readability, programs may
## use the fully-qualified `human-readable-title:ID` form
## for IDs.

type TitleId* = object
    title*: string
    id*: ID

converter toTitleId*(x: string): TitleId =
    ## Splits a string like "abc:def" into the title "abc" and ID ":def".
    let idx = x.find ':'
    if idx == -1: result.title = x
    else:
        result.title = x[0..<idx]
        if x[idx..x.high] != ":":
            # skip toId
            result.id = ID(x[idx..x.high])

proc uidMatches*(a: Doc, b: TitleId): bool =
    ## Returns true if a doc's ID matches.
    ## If both ID and title are specified, the ID must
    ## match *and* the doc must have a title that exactly
    ## matches as well.
    if b.id.len > 0:
        if a.key == b.id:
            if b.title.len > 0:
                for title in a.allTitles:
                    if title == b.title:
                        return true
            else:
                return true

proc titleMatchesExactly*(a: Doc, b: TitleId): bool =
    ## Returns true if the title matches the doc. If ID
    ## is specified, returns false -- use uidMatches
    ## instead.
    if b.id.len == 0:
        for title in a.allTitles:
            if title == b.title:
                return true

proc titleMatchesNormalized*(a: Doc, b: TitleId): bool =
    ## Returns true if the title "almost" matches the doc.
    ## If ID is specified, returns false -- use uidMatches
    ## instead.
    if b.id.len == 0:
        for title in a.allTitles:
            let atitle = title.replace("-").replace("_").toLowerAscii
            let btitle = b.title.replace("-").replace("_").toLowerAscii
            if atitle == btitle:
                return true

proc titleMatchesSubstring*(a: Doc, b: TitleID): bool =
    ## Returns true if the title is an exact substring of the other node
    if b.id.len == 0:
        for title in a.allTitles:
            let atitle = title.replace("-").replace("_").toLowerAscii
            let btitle = b.title.replace("-").replace("_").toLowerAscii
            if btitle in atitle:
                return true


proc titleMatchesFuzzy*(a: Doc, b: TitleID): bool =
    ## Returns true if all chars of the title appear in that order in the doc.
    if b.id.len == 0:
        for title in a.allTitles:
            let atitle = title.replace("-").replace("_").toLowerAscii
            let btitle = b.title.replace("-").replace("_").toLowerAscii
            var needle = 0
            # do all characters of btitle appear in atitle
            # in the same order?
            for hay in 0..atitle.high:
                if atitle[hay] == btitle[needle]:
                    needle += 1
                    if needle == btitle.len:
                        return true


proc onlyOneMatch(entries: HashSet[Doc],
                  title: string,
                  matchMethod: proc(a: Doc, b: TitleID): bool): Option[Doc] =
    ## Returns title's only match among vocabEntries, if it exists.
    ## matchMethod specifies how the matches are counted.
    ## If multiple matches are possible, returns nothing.
    var count = 0
    for doc in entries:
        if matchMethod(doc, title):
            result = some doc
            count += 1
    if count > 1:
        result = none(Doc)

const MATCH_METHODS = [
    uidMatches,
    titleMatchesExactly,
    titleMatchesNormalized,
    titleMatchesSubstring,
    titleMatchesFuzzy,
]

proc resolveDirectly*(vocab: Vocabulary, title: string, context: ID): Option[Doc] =
    ## Directly resolves `title` from the context's immediate vocab.
    ## If direct resolution isn't possible, returns none.
    ##
    ## Direct resolution considers only the set of Docs that directly follow `context`
    ## in the vocabulary, resolving to the first of:
    ## - the single Doc that matches the given UUID, otherwise
    ## - the single Doc with an exactly matching title, otherwise
    ## - the single Doc with a case-insensitively matching title, otherwise
    ## - the single Doc whose title contains the given title as a substring, otherwise
    ## - the single Doc whose title contains at least all of the letters of the given title in the order given, otherwise
    ## - direct resolution fails.
    ##
    ## If any of these sets match multiple Docs, direct resolution fails.
    for mm in MATCH_METHODS:
        if context in vocab:
            if Some(@vocabRule) ?= vocab.following(context).onlyOneMatch(title, mm):
                return some vocabRule

proc resolveIndirectly*(vocab: Vocabulary, title: string, context: ID): Option[seq[Doc]] =
    ## Indirect resolution is like direct resolution (see `resolveDirectly`), but
    ## it instead considers the set of every Doc that indirectly
    ## follows `context` in the vocabulary.
    ## This is more lenient, and may result in nested expressions to be
    ## structuralized into the given context.
    let pathsForFollowing = vocab.indirectlyFollowing(context)
    var following: HashSet[Doc]
    for k in pathsForFollowing.keys: following.incl k
    for mm in MATCH_METHODS:
        if Some(@vocabRule) ?= following.onlyOneMatch(title, mm):
            return some pathsForFollowing[vocabRule]
    return none(seq[Doc])

proc resolve*(vocab: Vocabulary, title: string, context: ID): Option[seq[Doc]] =
    ## The given title is resolved to the first node path that:
    ## - resolves to the title directly following `context` in the vocabulary, or
    ## - resolves to the title indirectly following `context` in the vocabulary.
    ## If neither resolves unambiguously (i.e. no Docs match the title or more than one Doc could match the title), resolution fails.
    if Some(@doc) ?= resolveDirectly(vocab, title, context):
        return some @[doc]
    if Some(@path) ?= resolveIndirectly(vocab, title, context):
        return some path
