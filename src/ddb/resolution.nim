import refSchema
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

proc uidMatches*(a: Expression, b: TitleId): bool =
  if b.id.len > 0:
    if a.key == b.id:
      if b.title.len > 0:
        for title in a.allTitles:
          if title == b.title:
            return true
      else:
        return true

proc titleMatchesExactly*(a: Expression, b: TitleId): bool =
  if b.id.len == 0:
    for title in a.allTitles:
        if title == b.title:
          return true

proc titleMatchesNormalized*(a: Expression, b: TitleId): bool =
  if b.id.len == 0:
    for title in a.allTitles:
      let atitle = title.replace("-").replace("_").toLowerAscii
      let btitle = b.title.replace("-").replace("_").toLowerAscii
      if atitle == btitle:
        return true

proc onlyMatch(schemaRules: HashSet[SchemaRule], title: string, matchMethod: proc(a: Expression, b: TitleID): bool): Option[Expression] =
  var count = 0
  for schemaRule in schemaRules:
    if matchMethod(schemaRule.expr, title):
      result = some schemaRule.expr
      count += 1
  if count > 1:
    result = none(Expression)

proc resolveDirectly*(schema: Schema, title: string, context: ID): Option[Expression] =
  for mm in [uidMatches, titleMatchesExactly, titleMatchesNormalized]:
    if context in schema:
      if Some(@schemaRule) ?= schema[context].children.onlyMatch(title, mm):
        return some schemaRule

proc resolveIndirectly*(schema: Schema, title: string, context: ID): Option[seq[Expression]] =
  var seen: HashSet[SchemaRule]
  var seenTwice: HashSet[SchemaRule]
  var paths: Table[Expression, seq[Expression]]
  var stack: seq[(SchemaRule, seq[Expression])]
  # quick DFS
  if context in schema:
    stack.add (schema[context], @[])
    while stack.len > 0:
      let (last, path) = stack.pop()
      seen.incl last
      paths[last.expr] = path
      for child in last.children:
        if child notin seen:
          stack.add (child, concat(path, @[child.expr]))
        else:
          seenTwice.incl child
    for mm in [uidMatches, titleMatchesExactly, titleMatchesNormalized]:
      if Some(@schemaRule) ?= (seen - seenTwice).onlyMatch(title, mm):
        return some paths[schemaRule]

proc resolve*(schema: Schema, title: string, context: ID): Option[seq[Expression]] =
  if Some(@expr) ?= resolveDirectly(schema, title, context):
    return some @[expr]
  if Some(@path) ?= resolveIndirectly(schema, title, context):
    return some path
