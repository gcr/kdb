import docs
import sequtils
import tables
import sets
import strutils
import strformat
import hashes
import tables
import options
import fusion/matching

type
  Vocabulary* = Table[ID, HashSet[Doc]]
  #VocabEntry* = ref object
  #  doc*: Doc
  #  children*: HashSet[VocabEntry] ## mapping titles to these objects
proc hash*(expr: Expr): Hash =
  result = result !& expr.kind.hash !& expr.val.hash !& expr.children.hash
  result = !$result
#proc hash*(sr: VocabEntry): Hash =
#  result = !$ (result !& sr.doc.hash)
proc `$`*(vocab: Vocabulary): string =
  ## for debugging
  for k, vals in vocab.pairs:
    result &= fmt"- {$k} "
    for val in vals:
      result &= val.allTitles.toSeq.join(",") #& ":" & $val.key
      result &= " "
    result &= "\n"



method getFullVocabulary*(library: Library): Vocabulary {.base.} =
  template addVocab(k: ID, d: Doc): untyped =
    ### Indicate that d directly follows k
    if k notin result:
      result[k] = HashSet[Doc]()
    if d.key notin result:
      result[d.key] = HashSet[Doc]()
    result[k].incl d
  var docCache: Table[ID, Doc]
  proc lookupDoc(key: ID): Option[Doc] =
    if key notin docCache:
      if Some(@doc) ?= library.lookup key:
        docCache[key] = doc
        return some doc

  ## Docs with (vocab-for ":something") can nest inside :something
  ## expressions
  for nextDoc in library.searchFor vocabFor.key:
    for prev in nextDoc / vocabFor:
      addVocab(toId(prev.val), nextDoc)
  ## Docs with (vocab-has ":something") can contain :something
  ## expressions. Typically these also need to be "vocab for"
  ## some other node to be resolved anywhere.
  for prevDoc in library.searchFor vocabHas.key:
    for next in prevDoc / vocabHas:
      if Some(@nextDoc) ?= lookupDoc(next.val.toId):
        addVocab(prevDoc.key, nextDoc)
