import refSchema
import options
import strutils
import fusion/matching
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

proc uidMatches*(a: Ref, b: TitleId): bool =
  if b.id.len > 0:
    if a.reflink == b.id:
      if b.title.len > 0:
        for title in a.titles:
          if title == b.title:
            return true
      else:
        return true

proc titleMatchesExactly*(a: Ref, b: TitleId): bool =
  if b.id.len == 0:
    for title in a.titles:
        if title == b.title:
          return true

proc titleMatchesNormalized*(a: Ref, b: TitleId): bool =
  if b.id.len == 0:
    for title in a.titles:
      let atitle = title.replace("-").replace("_").toLowerAscii
      let btitle = b.title.replace("-").replace("_").toLowerAscii
      if atitle == btitle:
        return true

proc resolveDirectly*(uni: Universe, title: string, context: ID): Option[Ref] =
  for matchMethod in [
                      uidMatches,
                      titleMatchesExactly,
                      titleMatchesNormalized
  ]:
    var count = 0
    for field in uni.fieldsFor context:
      if matchMethod(field, title):
          result = some field
          count += 1
    if count != 1:
      result = none(Ref)
    if result.isSome:
      return result


proc resolve*(uni: Universe, title: string, context: ID): Option[Ref] =
  resolveDirectly(uni, title, context)