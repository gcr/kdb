import ddb/refSchema
import ddb/dexpParsing
import unittest
import sequtils
import options
import strutils
import ddb/resolution

suite "Bulit-in refs":
  test "Essential ref operations":
    check:
      📝Field.has 📝Title
      📝Field.has 📝Title, "Field"
      not 📝Field.has(📝Title, "NotField")
      📝Field.title == "Field"
      payload(📝Field // 📝Title) == some("Field")
    let allBuiltinFields = builtins.fieldsFor("").toSeq.mapIt(it.title)
    check allBuiltinFields == @["Title", "Field"]

  test "Check titles of child dexpr refs":
    let childrenRefs: seq[Ref] = 📝Field.toSeq.mapIt(
      builtins.lookup(it).get())
    let titles = childrenRefs.mapIt((it // 📝Title).payload)
    check titles == @[some "Field", some "Title"]
    let titles2 = childrenRefs.mapIt it.title
    check titles2 == @["Field", "Title"]

  test "Multiple children":
    let x = newRef "multichild":
      📝Title "one"
      📝Title "two"
    check x.title == "one"
    check (x / 📝Title).toSeq.payloads == @["one", "two"]
    check x.has(📝Title)
    check not x.has(📝Field)
    check x.has(📝Title, "one")
    check x.has(📝Title, "two")
    check x.titles == @["one", "two"]

  test "Deeply nested refs":
    let
      head = newRef "head": 📝Field
      author = newRef "auth": 📝Field "head"
      date = newRef "date": 📝Field "head"
      someDoc = newRef "someDoc":
          head:
            author "Kimmmy"
            date "2024"
    check ((someDoc // head).get() // author).payload == some "Kimmmy"
    check payload(someDoc // head // date) == some "2024"
    check payload(someDoc // date) == none(string)


  test "Builtins is populated":
    check builtins.contains 📝Field
    check builtins.contains 📝Title

  test "Making ephemeral refs doesn't touch builtins":
    let x = newRef "abc": 📝Title "something"
    check:
      not builtins.contains x
      payload(x // 📝Title) == some "something"
      📝Title // x == none(Dexpr)
      payload(📝Title // x) == none(string)

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
        doc = newRef "dd":
            📝Title "Doc"
            📝Field
        head = newRef "uuidHead":
            📝Title "Head"
            📝Field
        date = newRef "uuidDate":
            📝Title "Date"
            📝Field "uuidHead"
        title = newRef "titleButInsideHead":
            📝Title "Title"
            📝Field "uuidHead"
        dup1 = newRef "someDup1":
            📝Title "DuplicateName"
            📝Field "uuidHead"
        dup2 = newRef "someDup2":
            📝Title "DuplicateName"
            📝Title "UnambiguousName"
            📝Field "uuidHead"
    var univ = newMapUniverse()
    univ.add doc, head, date, title, dup1, dup2

  test "Unpolluted builtins":
    check doc in univ
    check doc notin builtins

  test "ID parsing":
    check "foo".toTitleId == TitleId(title: "foo", id: "")
    check "foo$".toTitleId == TitleId(title: "foo", id: "")
    check "$abcd".toTitleId == TitleId(title: "", id: "abcd")
    check "a$b".toTitleId == TitleId(title: "a", id: "b")
    check "$".toTitleId == TitleId(title: "", id: "")

  test "Direct resolution":
    check:
      univ.resolveDirectly("Doc", context="").isSome
      univ.resolveDirectly("NoMatch", context="").isNone
      univ.resolveDirectly("Title", context="uuidHead").reflink == some "titleButInsideHead"
      univ.resolveDirectly("Title", context="").reflink == some 📝Title.reflink
      univ.resolveDirectly("Date", context="").reflink == none(ID)
      univ.resolveDirectly("Date", context="uuidHead").reflink == some "uuidDate"
      univ.resolveDirectly("DuplicateName", context="uuidHead").isNone
      univ.resolveDirectly("$someDup2", context="uuidHead").reflink == some "someDup2"
      univ.resolveDirectly("NONMATCHING$someDup2", context="uuidHead").isNone
      univ.resolveDirectly("DuplicateName$someDup1", context="uuidHead").reflink == some "someDup1"
      univ.resolveDirectly("$someDup2", context="").isNone
      univ.resolveDirectly("UnambiguousName", context="uuidHead").reflink == some "someDup2"
      univ.resolveDirectly("unambiguous-name", context="uuidHead").reflink == some "someDup2"
      univ.resolveDirectly("UNAMBIGUOUS_NAME", context="uuidHead").reflink == some "someDup2"


