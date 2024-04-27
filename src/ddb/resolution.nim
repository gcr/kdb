import refSchema
import options
import strutils
import fusion/matching
import tables
import sets
{.experimental: "caseStmtMacros".}

type TitleId* = object
    title*: string
    id*: string

converter toTitleId*(x: string): TitleId =
    let idx = x.find '$'
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

proc resolveDirectly*(schema: Schema, title: string, context: ID): Option[Expression] =
  for matchMethod in [
                      uidMatches,
                      titleMatchesExactly,
                      titleMatchesNormalized
  ]:
    var count = 0
    if context in schema:
      for schemaRule in schema[context].children:
        if matchMethod(schemaRule.expr, title):
            result = some schemaRule.expr
            count += 1
    if count != 1:
      result = none(Expression)
    if result.isSome:
      return result


proc resolve*(schema: Schema, title: string, context: ID): Option[Expression] =
  resolveDirectly(schema, title, context)