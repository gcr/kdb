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
    id*: string

converter toTitleId*(x: string): TitleId =
    let idx = x.find ':'
    if idx == -1: result.title = x
    else:
        result.title = x[0..<idx]
        result.id = x[idx+1..x.high]

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

proc onlyMatch(vocabEntries: HashSet[VocabEntry], title: string, matchMethod: proc(a: Doc, b: TitleID): bool): Option[Doc] =
  var count = 0
  for vocabEntry in vocabEntries:
    if matchMethod(vocabEntry.doc, title):
      result = some vocabEntry.doc
      count += 1
  if count > 1:
    result = none(Doc)

proc resolveDirectly*(vocab: Vocabulary, title: string, context: ID): Option[Doc] =
  for mm in [uidMatches, titleMatchesExactly, titleMatchesNormalized]:
    if context in vocab:
      if Some(@schemaRule) ?= vocab[context].children.onlyMatch(title, mm):
        return some schemaRule

proc resolveIndirectly*(vocab: Vocabulary, title: string, context: ID): Option[seq[Doc]] =
  var seen: HashSet[VocabEntry]
  var seenTwice: HashSet[VocabEntry]
  var paths: Table[Doc, seq[Doc]]
  var stack: seq[(VocabEntry, seq[Doc])]
  # quick DFS
  if context in vocab:
    stack.add (vocab[context], @[])
    while stack.len > 0:
      let (last, path) = stack.pop()
      seen.incl last
      paths[last.doc] = path
      for child in last.children:
        if child notin seen:
          stack.add (child, concat(path, @[child.doc]))
        else:
          seenTwice.incl child
    for mm in [uidMatches, titleMatchesExactly, titleMatchesNormalized]:
      if Some(@schemaRule) ?= (seen - seenTwice).onlyMatch(title, mm):
        return some paths[schemaRule]

proc resolve*(vocab: Vocabulary, title: string, context: ID): Option[seq[Doc]] =
  if Some(@doc) ?= resolveDirectly(vocab, title, context):
    return some @[doc]
  if Some(@path) ?= resolveIndirectly(vocab, title, context):
    return some path
