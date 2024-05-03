import vocabulary
import docs
import options
import strutils
import fusion/matching
import tables
import hashes
import sets
import sequtils
{.experimental: "caseStmtMacros".}

type TitleId* = object
    title*: string
    id*: ID

converter toTitleId*(x: string): TitleId =
    let idx = x.find ':'
    if idx == -1: result.title = x
    else:
        result.title = x[0..<idx]
        if x[idx..x.high] != ":":
          result.id = toId(x[idx..x.high])

proc uidMatches*(a: Doc, b: TitleId): bool =
  if b.id.len > 0:
    if a.key == b.id:
      if b.title.len > 0:
        for title in a.allTitles:
          if title == b.title:
            return true
      else:
        return true

proc titleMatchesExactly*(a: Doc, b: TitleId): bool =
  if b.id.len == 0:
    for title in a.allTitles:
        if title == b.title:
          return true

proc titleMatchesNormalized*(a: Doc, b: TitleId): bool =
  if b.id.len == 0:
    for title in a.allTitles:
      let atitle = title.replace("-").replace("_").toLowerAscii
      let btitle = b.title.replace("-").replace("_").toLowerAscii
      if atitle == btitle:
        return true

proc onlyMatch(vocabEntries: HashSet[Doc], title: string, matchMethod: proc(a: Doc, b: TitleID): bool): Option[Doc] =
  var count = 0
  for doc in vocabEntries:
    if matchMethod(doc, title):
      result = some doc
      count += 1
  if count > 1:
    result = none(Doc)

proc resolveDirectly*(vocab: Vocabulary, title: string, context: ID): Option[Doc] =
  for mm in [uidMatches, titleMatchesExactly, titleMatchesNormalized]:
    if context in vocab:
      if Some(@vocabRule) ?= vocab[context].onlyMatch(title, mm):
        return some vocabRule

proc resolveIndirectly*(vocab: Vocabulary, title: string, context: ID): Option[seq[Doc]] =
  var seen: HashSet[Doc]
  var seenTwice: HashSet[Doc]
  var paths: Table[Doc, seq[Doc]]
  var stack: seq[(Doc, seq[Doc])]
  # quick DFS
  if context in vocab:
    for nextVocab in vocab[context]:
      stack.add (nextVocab, @[nextVocab])
    while stack.len > 0:
      let (last, path) = stack.pop()
      seen.incl last
      paths[last] = path
      for nextVocab in vocab[last.key]:
        if nextVocab notin seen:
          if nextVocab.key != ":top".toID:
            stack.add (nextVocab, concat(path, @[nextVocab]))
        else:
          seenTwice.incl nextVocab
    for mm in [uidMatches, titleMatchesExactly, titleMatchesNormalized]:
      if Some(@vocabRule) ?= (seen - seenTwice).onlyMatch(title, mm):
        return some paths[vocabRule]
  return none(seq[Doc])

proc resolve*(vocab: Vocabulary, title: string, context: ID): Option[seq[Doc]] =
  if Some(@doc) ?= resolveDirectly(vocab, title, context):
    return some @[doc]
  if Some(@path) ?= resolveIndirectly(vocab, title, context):
    return some path
