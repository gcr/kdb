import docs
import sequtils
import tables
import sets
import strutils
import strformat
import hashes
import tables

type
  Vocabulary* = Table[string, HashSet[Doc]]
  #VocabEntry* = ref object
  #  doc*: Doc
  #  children*: HashSet[VocabEntry] ## mapping titles to these objects
proc hash*(expr: Expr): Hash =
  result = result !& expr.kind.hash !& expr.val.hash !& expr.children.hash
  result = !$result
#proc hash*(sr: VocabEntry): Hash =
#  result = !$ (result !& sr.doc.hash)
proc `$`*(schema: Vocabulary): string =
  ## for debugging
  for k, vals in schema.pairs:
    result &= fmt"- {$k}: "
    for val in vals:
      result &= val.allTitles.toSeq.join(",") #& ":" & $val.key
      result &= " "
    result &= "\n"



method getSchema*(library: Library): Vocabulary {.base.} =
  template ensureExists(key: ID): untyped =
    discard result.mgetOrPut(key, HashSet[Doc]())
  for doc in library.searchFor docs.vocab.key:
    ensureExists(doc.key)
    for allowedParent in doc / docs.vocab:
      ensureExists(allowedParent.val)
      result[allowedParent.val].incl doc