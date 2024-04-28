import ddb/refSchema
import ddb/dexpParsing
import ddb/resolution
import unittest
import sequtils
import options
import strutils
import tables
import fusion/matching

suite "Bulit-in refs":
  test "Essential ref operations":
    check:
      annotates.has title
      annotates.has title, "Annotates"
      not annotates.has(title, "notAnnotates")
      annotates.allTitles == @["Annotates"]
      annotates.firstTitle == some "Annotates"

  test "Check titles of child annotations":
    let exprs: seq[Expression] = annotates.items.toSeq.mapIt(
      builtins.lookup(it.kind).get())
    let titles = exprs.mapIt(it.firstTitle)
    check titles == @[some "Annotates", some "Title"]

  test "Multiple children":
    let x = newExpression "multichild":
      title "one"
      title "two"
    check x.firstTitle == some "one"
    check (x / title).toSeq.mapIt(it.val) == @["one", "two"]
    check x.has title
    check not x.has annotates
    check x.has(title, "one")
    check x.has(title, "two")
    check x.allTitles == @["one", "two"]

  test "Deeply nested refs":
    let
      head = newExpression "head": annotates ""
      author = newExpression "auth": annotates "head"
      date = newExpression "date": annotates "head"
      someDoc = newExpression "someDoc":
          head:
            author "Kimmmy"
            date "2024"
    var authors: seq[string]
    for h in someDoc / head:
      for aut in h / author:
        authors.add aut.val
    check authors == @["Kimmmy"]
    check ((someDoc / head).toSeq[0] / date).toSeq[0].val == "2024"
    var shouldntHave: bool
    for x in someDoc / date:
      shouldntHave = true
    check not shouldntHave


  test "Builtins is populated":
    check builtins.contains annotates.key
    check builtins.contains annotates.key

  test "Making ephemeral refs doesn't touch builtins":
    let x = newExpression "abc": title "something"
    check:
      @["something"] == x.allTitles
      not title.has(x)

suite "Dexpr parsing":
  proc tokenize(input: string): string =
    let tokens = input.parseToUnresolvedTokenStream.mapIt($it)
    return tokens.join(" ")

  test "Parse streams":
    check:
      tokenize("abc def ghi")==
         "bare(abc) bare(def) bare(ghi)"
      tokenize("abc, def, ghi")==
         "bare(abc) bare(def) bare(ghi)"
      tokenize("(foo, b$ar, (ba-z q_ux))")==
          "push(foo) bare(b$ar) push(ba-z) bare(q_ux) pop pop"
      tokenize("(one two(three, four(), five(six)))")==
          "push(one) push(two) bare(three) push(four) "&
          "pop push(five) bare(six) pop pop pop"
      tokenize("x(y((z(w))")==
          "push(x) push(y) push(z) push(w) pop pop"
      tokenize("()")==""
      tokenize("() x")==""
      tokenize("\"juice\"")==""
      tokenize("(\"juice\"")==""
      tokenize("(foo")=="push(foo)"
      tokenize("(foo bar")==
        "push(foo) bare(bar)"
      tokenize("(foo \"uh oh")==
        "push(foo) stri(uh oh)"
      tokenize("(foo \"strlit\" bar \"aaa\"")==
        "push(foo) str(strlit) bare(bar) str(aaa)"
      tokenize("(foo \"strlit\" \"aaa\"")==
        "push(foo) str(strlit) str(aaa)"
      tokenize("(foo,,,bar")=="push(foo) bare(bar)"
      tokenize("(foo,,,(,,bar")=="push(foo) push(bar)"
      tokenize("(foo bar(")=="push(foo) push(bar)"
      tokenize("(foo (bar")=="push(foo) push(bar)"
      tokenize("(foo \"bad")=="push(foo) stri(bad)"

  proc parseString(str:string):string =
    let tokens = parseToUnresolvedTokenStream("(open " & str)
    if ($tokens[1]).startsWith("str"):
      return ($tokens[1])[4..^2]
    else:
      # something WAY wrong happened
      return $tokens[1]
  test "String escapes":
    check:
      # note use of raw strings and escaping gymnastics
      parseString("\"hello world\"")==
        "hello world"
      parseString("\"she said \\\"hello\\\"\"")==
        "she said \"hello\""

      # from json.org
      parseString("\" quote: \\\" \"")==
        " quote: \" "
      parseString("\" backslash: \\\\ \"")==
        " backslash: \\ "
      parseString("\" slash: \\/ \"")==
        " slash: / "
      parseString("\" backspace: \\b \"")==
        " backspace: \b "
      parseString("\" form feed: \\f \"")==
        " form feed: \f "
      parseString("\" newline: \\n \"")==
        " newline: \n "
      parseString("\" cr: \\r \"")==
        " cr: \r "
      parseString("\" tab: \\t \"")==
        " tab: \t "
      parseString("\" unicode: \\uf2F3 \"")==
        " unicode: \uf2f3 "
      parseString("\" in a row: \\r\\n\\b\\u0000 \" ")==
        " in a row: \r\n\b\u0000 "
      parseString("\"\\n\\n\\n\"")=="\n\n\n"

      # Raw binary escapes !!
      parseString("\" Raw: \\R 1 x\\R 5 \x00\x01\"\x02\xf3\\n \"") ==
        " Raw: x\x00\x01\"\x02\xf3\n "
      parseString("\"\\R 1 x\"")=="x"
      parseString("\"\\R 2 xxx\"")=="xxx"
      parseString("\"\\R 12 asdfghjklqwe\"")=="asdfghjklqwe"
      tokenize("(foo \"\\R 100 incomplete\"")=="push(foo) str(incomplete)"

suite "Ref resolution":
  setup:
    let
        travel = newExpression "travel":
            title "Travel"
            annotates ""
        doc = newExpression "dd":
            title "Doc"
            annotates ""
        head = newExpression "uuidHead":
            title "Head"
            annotates "dd"
        date = newExpression "uuidDate":
            title "Date"
            annotates "uuidHead"
        innertitle = newExpression "titleButInsideHead":
            title "Title"
            annotates "uuidHead"
        dup1 = newExpression "someDup1":
            title "DuplicateName"
            annotates "uuidHead"
        dup2 = newExpression "someDup2":
            title "DuplicateName"
            title "UnambiguousName"
            annotates "uuidHead"
        dup3 = newExpression "someDup3":
            title "NameMatchMethodTest"
            annotates "uuidHead"
        dup4 = newExpression "someDup4":
            title "name_match_method_test"
            annotates "uuidHead"
    var univ = newMapUniverse()
    univ.add doc, head, date, innertitle, dup1, dup2, dup3, dup4, travel

  test "Unpolluted builtins":
    check doc.key in univ
    check doc.key notin builtins

  test "ID parsing":
    check "foo".toTitleId == TitleId(title: "foo", id: "")
    check "foo$".toTitleId == TitleId(title: "foo", id: "")
    check "$abcd".toTitleId == TitleId(title: "", id: "abcd")
    check "a$b".toTitleId == TitleId(title: "a", id: "b")
    check "$".toTitleId == TitleId(title: "", id: "")

  test "Direct resolution":
    let schema = univ.getSchema()
    check:
      schema.resolveDirectly("Doc", context="").isSome
      schema.resolveDirectly("NoMatch", context="").isNone
      schema.resolveDirectly("Title", context="uuidHead").key == some "titleButInsideHead"
      schema.resolveDirectly("Title", context="").key == some title.kind
      schema.resolveDirectly("Date", context="").key == none(ID)
      schema.resolveDirectly("Date", context="uuidHead").key == some "uuidDate"
      schema.resolveDirectly("DuplicateName", context="uuidHead").isNone
      schema.resolveDirectly("$someDup2", context="uuidHead").key == some "someDup2"
      schema.resolveDirectly("NONMATCHING$someDup2", context="uuidHead").isNone
      schema.resolveDirectly("DuplicateName$someDup1", context="uuidHead").key == some "someDup1"
      schema.resolveDirectly("$someDup2", context="").isNone
      schema.resolveDirectly("UnambiguousName", context="uuidHead").key == some "someDup2"
      schema.resolveDirectly("unambiguous-name", context="uuidHead").key == some "someDup2"
      schema.resolveDirectly("UNAMBIGUOUS_NAME", context="uuidHead").key == some "someDup2"

  test "Resolution order should matter":
    let schema = univ.getSchema()
    check:
      schema.resolveDirectly("name-matchMethod_test", context="uuidHead").isNone
      schema.resolveDirectly("name_match_method_test", context="uuidHead").key == some "someDup4"
      schema.resolveDirectly("NameMatchMethodTest", context="uuidHead").key == some "someDup3"
      schema.resolveDirectly("nameMatchMethodTest", context="uuidHead").isNone
  test "Indirect resolution":
    let travelHead = newExpression "recDup":
      title "RecursiveDuplicate"
      annotates "travel"
      annotates "dd"
      annotates "uuidHead"
    univ.add travelHead
    let schema = univ.getSchema()
    proc resolveIndirectly(title: string, context: ID): Option[seq[string]] =
      if Some(@path) ?= schema.resolveIndirectly(title, context):
        return some path.mapIt(it.key)
    check:
      resolveIndirectly("name-matchMethod_test", context="").isNone
      resolveIndirectly("name-matchMethod_test", context="dd").isNone
      resolveIndirectly("unambiguousName", context="") == some @["dd", "uuidHead", "someDup2"]
      resolveIndirectly("unambiguous-name", context="") == some @["dd", "uuidHead", "someDup2"]
      resolveIndirectly("unambiguousName", context="dd") == some @["uuidHead", "someDup2"]
      resolveIndirectly("unambiguousName", context="travel").isNone

      # only one node appearing in many contexts
      resolveIndirectly("RecursiveDuplicate", context="").isNone
      resolveIndirectly("RecursiveDuplicate", context="dd").isNone
      resolveIndirectly("RecursiveDuplicate", context="travel").isSome
      resolveIndirectly("RecursiveDuplicate", context="travel") == some @["recDup"]
      resolveIndirectly("RecursiveDuplicate", context="uuidHead") == some @["recDup"]


