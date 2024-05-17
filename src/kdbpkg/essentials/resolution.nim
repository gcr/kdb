import vocabulary
import docs
import options
import strutils
import fusion/matching
import tables
import hashes
import sets
{.experimental: "caseStmtMacros".}

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

proc resolveDirectly*(vocab: Vocabulary, title: string, context: ID): Option[Doc] =
    ## Directly resolves `title` from the context's immediate vocab.
    ## If direct resolution isn't possible, returns none.
    for mm in [uidMatches, titleMatchesExactly, titleMatchesNormalized]:
        if context in vocab:
            if Some(@vocabRule) ?= vocab.following(context).onlyOneMatch(title, mm):
                return some vocabRule

proc resolveIndirectly*(vocab: Vocabulary, title: string, context: ID): Option[seq[Doc]] =
    let pathsForFollowing = vocab.indirectlyFollowing(context)
    var following: HashSet[Doc]
    for k in pathsForFollowing.keys: following.incl k
    for mm in [uidMatches, titleMatchesExactly, titleMatchesNormalized]:
        if Some(@vocabRule) ?= following.onlyOneMatch(title, mm):
            return some pathsForFollowing[vocabRule]
    return none(seq[Doc])

proc resolve*(vocab: Vocabulary, title: string, context: ID): Option[seq[Doc]] =
    if Some(@doc) ?= resolveDirectly(vocab, title, context):
        return some @[doc]
    if Some(@path) ?= resolveIndirectly(vocab, title, context):
        return some path
