import docs
import sequtils
import tables
import sets
import strutils
import strformat
import hashes
import options
import fusion/matching

type
  ImmediateVocabEntries* = object
    doc*: Option[Doc]
    children: HashSet[Doc]

  Vocabulary* = Table[ID, ImmediateVocabEntries]
  ## Maps IDs to the set of Docs that can immediately follow them.
  ## Example:
  ##   let a: Vocabulary = {":b": Doc(":c"), ...}
  ## will let (:b (:c)) resolve directly

proc hash*(expr: Expr): Hash =
  result = result !& expr.kind.hash !& expr.val.hash !& expr.children.hash
  result = !$result
proc `$`*(vocab: Vocabulary): string =
  ## for debugging
  for k, vals in vocab.pairs:
    result &= fmt"- {$k} "
    for val in vals.children:
      result &= val.allTitles.toSeq.join(",") #& ":" & $val.key
      result &= " "
    result &= "\n"

proc following*(vocab: Vocabulary, id: ID, cycleBreak: seq[ID] = @[]): HashSet[Doc] =
  ## Return all vocab that could be directly resolved under id.
  ## Handles `vocab-same-as` properly.
  if id in vocab:
    for child in vocab[id].children:
      result.incl child
    if Some(@doc) ?= vocab[id].doc:
      # Handle vocab-same-as
      for sameVocab in doc / vocabSameAs:
        if Some(@sameVocabId) ?= sameVocab.val.toID:
          if sameVocabId notin cycleBreak:
            for child in vocab.following(sameVocabId, cycleBreak & @[sameVocabId]):
              result.incl child

proc indirectlyFollowing*(vocab: Vocabulary, context: ID): Table[Doc, seq[Doc]] =
  ## List all docs that could possibly appear in `context`'s vocabulary.
  ## The return value maps a doc to the path taken to get there via direct
  ## resoltuion.
  var seenTwice: HashSet[Doc]
  var stack: seq[(Doc, seq[Doc])]
  # quick DFS to accumulate possibilities
  if context in vocab:
      for nextVocab in vocab.following context:
          stack.add (nextVocab, @[nextVocab])
      while stack.len > 0:
          let (last, path) = stack.pop()
          result[last] = path
          for nextVocab in vocab.following last.key:
              if nextVocab notin result:
                  if nextVocab.key != ID":top":
                      stack.add (nextVocab, concat(path, @[nextVocab]))
              else:
                  seenTwice.incl nextVocab
  for doc in seenTwice:
    result.del doc

method getFullVocabulary*(library: Library): Vocabulary {.base.} =
  template ensureExists(k: ID): untyped =
    if k notin result:
      result[k] = ImmediateVocabEntries(doc: library.lookup(k))
  template ensureExists(d: Doc): untyped =
    ensureExists(d.key)
    if result[d.key].doc.isNone:
      result[d.key].doc = some d
  template addVocab(k: ID, d: Doc): untyped =
    # Indicate that d directly follows k.
    ensureExists k
    ensureExists d
    result[k].children.incl d

  ## Docs with (vocab-for ":something") can nest inside :something
  ## expressions
  for nextDoc in library.searchFor vocabFor.key:
    for prev in nextDoc / vocabFor:
      if Some(@id) ?= prev.val.toId:
        addVocab(id, nextDoc)

  ## Docs with (vocab-has ":something") can contain :something
  ## expressions. Typically these also need to be "vocab for"
  ## some other node to be resolved anywhere.
  for prevDoc in library.searchFor vocabHas.key:
    for next in prevDoc / vocabHas:
      if Some(@nextId) ?= next.val.toId:
        ensureExists(nextId)
        if Some(@nextDoc) ?= result[nextId].doc:
          addVocab(prevDoc.key, nextDoc)
