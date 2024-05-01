import docs
import sequtils
import tables
import sets
import strutils
import strformat
import hashes
import tables

type
  Vocabulary* = Table[string, VocabEntry]
  VocabEntry* = ref object
    doc*: Doc
    children*: HashSet[VocabEntry] ## mapping titles to these objects
proc hash*(expr: Expr): Hash =
  result = result !& expr.kind.hash !& expr.val.hash !& expr.children.hash
  result = !$result
proc hash*(sr: VocabEntry): Hash =
  result = !$ (result !& sr.doc.hash)
proc `$`*(schema: VocabEntry): string =
  ## for debugging
  let title = schema.doc.allTitles.toSeq.join ","
  result = fmt"({title}"
  for child in schema.children:
    result &= fmt" {$child}"
  result &= ")"



method getSchema*(library: Library): Vocabulary {.base.} =
  template ensureExists(key: ID): untyped =
    result.mgetOrPut(key, VocabEntry())
  for doc in library.searchFor docs.vocab.key:
    var schemarule = ensureExists(doc.key)
    schemarule.doc = doc
    for allowedParent in doc / docs.vocab:
      discard ensureExists(allowedParent.val)
      result[allowedParent.val].children.incl result[doc.key]