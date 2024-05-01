import kdbpkg/essentials/[docs, vocabulary, parsing, resolution, repr]
import kdbpkg/libraries/sqliteLibrary
import fusion/matching
import std/unittest
import std/sequtils
import std/options
import std/strutils
import std/strformat
import std/tempfiles
import std/files
import std/paths

suite "Bulit-in refs":
  test "Essential ref operations":
    check:
      vocab.has title
      vocab.hasEqual title, "vocab"
      not vocab.hasEqual(title, "something else")
      vocab.allTitles == @["vocab"]
      vocab.firstTitle == some "vocab"

  test "Check titles of child Exprs":
    let docs: seq[Doc] = vocab.items.toSeq.mapIt(
      builtins.lookup(it.kind).get())
    let titles = docs.mapIt(it.firstTitle)
    check titles == @[some "vocab", some "title"]

  test "Multiple children":
    let x = newDoc "multichild":
      title "one"
      title "two"
    check x.firstTitle == some "one"
    check (x / title).toSeq.mapIt(it.val) == @["one", "two"]
    check x.has title
    check not x.has vocab
    check x.hasEqual(title, "one")
    check x.hasEqual(title, "two")
    check x.allTitles == @["one", "two"]

  test "Deeply nested refs":
    let
      head = newDoc "head": vocab ""
      author = newDoc "auth": vocab "head"
      date = newDoc "date": vocab "head"
      someDoc = newDoc "someDoc":
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
    check builtins.contains vocab.key
    check builtins.contains title.key

  test "Making ephemeral refs doesn't touch builtins":
    let x = newDoc "abc": title "something"
    check:
      @["something"] == x.allTitles
      not title.has(x)

suite "Dexpr parsing":
  proc tokenize(input: string): string =
    var parser = ParseState(input: input)
    parser.processTokenStream()
    let tokens = parser.tokens.mapIt($it)
    return tokens.join(" ")

  test "Parse streams":
    check:
      tokenize("abc def ghi")==
         "bare(abc) bare(def) bare(ghi)"
      tokenize("abc, def, ghi")==
         "bare(abc) bare(def) bare(ghi)"
      tokenize("(foo, b:ar, (ba-z q_ux))")==
          "push(foo) bare(b:ar) push(ba-z) bare(q_ux) pop pop"
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
      tokenize("(foo \"strlit\" bar \"aaa\" \"bbb\"")==
        "push(foo) str(strlit) bare(bar) str(aaa) str(bbb)"
      tokenize("(foo \"strlit\" \"aaa\"")==
        "push(foo) str(strlit) str(aaa)"
      tokenize("(foo,,,bar")=="push(foo) bare(bar)"
      tokenize("(foo,,,(,,bar")=="push(foo) push(bar)"
      tokenize("(foo bar(")=="push(foo) push(bar)"
      tokenize("(foo (bar")=="push(foo) push(bar)"
      tokenize("(foo \"bad")=="push(foo) stri(bad)"

  proc parseString(str:string):string =
    var parser = ParseState(input: "(open "&str)
    parser.processTokenStream()
    if ($parser.tokens[1]).startsWith("str"):
      return ($parser.tokens[1])[4..^2]
    else:
      # something WAY wrong happened
      return $parser.tokens[1]
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
        travel = newDoc "travel":
            title "Travel"
            vocab ""
        doc = newDoc "dd":
            title "Doc"
            vocab ""
        head = newDoc "uuidHead":
            title "Head"
            vocab "dd"
        date = newDoc "uuidDate":
            title "Date"
            vocab "uuidHead"
        innertitle = newDoc "titleButInsideHead":
            title "Title"
            vocab "uuidHead"
        dup1 = newDoc "someDup1":
            title "DuplicateName"
            vocab "uuidHead"
        dup2 = newDoc "someDup2":
            title "DuplicateName"
            title "UnambiguousName"
            vocab "uuidHead"
        dup3 = newDoc "someDup3":
            title "NameMatchMethodTest"
            vocab "uuidHead"
        dup4 = newDoc "someDup4":
            title "name_match_method_test"
            vocab "uuidHead"
    var univ = newMapLibrary()
    univ.add doc, head, date, innertitle, dup1, dup2, dup3, dup4, travel

  test "Unpolluted builtins":
    check doc.key in univ
    check doc.key notin builtins

  test "ID parsing":
    check "foo".toTitleId == TitleId(title: "foo", id: "")
    check "foo:".toTitleId == TitleId(title: "foo", id: "")
    check ":abcd".toTitleId == TitleId(title: "", id: "abcd")
    check "a:b".toTitleId == TitleId(title: "a", id: "b")
    check ":".toTitleId == TitleId(title: "", id: "")

  test "Direct resolution":
    let schema = univ.getSchema()
    check:
      schema.resolveDirectly("Doc", context="").isSome
      schema.resolveDirectly("NoMatch", context="").isNone
      schema.resolveDirectly("Title", context="uuidHead").key == some "titleButInsideHead"
      schema.resolveDirectly("Title", context="").key == some title.key
      schema.resolveDirectly("Date", context="").key == none(ID)
      schema.resolveDirectly("Date", context="uuidHead").key == some "uuidDate"
      schema.resolveDirectly("DuplicateName", context="uuidHead").isNone
      schema.resolveDirectly(":someDup2", context="uuidHead").key == some "someDup2"
      schema.resolveDirectly("NONMATCHING:someDup2", context="uuidHead").isNone
      schema.resolveDirectly("DuplicateName:someDup1", context="uuidHead").key == some "someDup1"
      schema.resolveDirectly("$:omeDup2", context="").isNone
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
    let travelHead = newDoc "recDup":
      title "RecursiveDuplicate"
      vocab "travel"
      vocab "dd"
      vocab "uuidHead"
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

suite "Structuralization":
  setup:
    var univ = newMapLibrary()
    univ.add: newDoc "doc": title "doc"; vocab ""
    univ.add: newDoc "head": title "head"; vocab "doc"
    univ.add: newDoc "author": title "author"; vocab "head"
    univ.add: newDoc "date": title "date"; vocab "head"
    univ.add: newDoc "body": title "body"; vocab "doc"
    univ.add: newDoc "h1": title "h1"; vocab "body"
    univ.add: newDoc "span": title "span"; vocab "h1"; vocab "body"
    univ.add: newDoc "dup1": title "dup"; vocab "body"; vocab "body"
    univ.add: newDoc "dup2": title "dup"; title "dup2"; vocab "body"; vocab "body"
    proc structure(str: string): string =
      var parser = ParseState(input: str, vocab: univ.getSchema())
      parser.processTokenStream()
      parser.structuralize("")
      case parser:
      of Ok(tokens: @tokens): return tokens.mapIt($it).join(" ")
      of Fail(loc: @loc, message: @msg): return fmt "{loc}: {msg}"
    proc parseAnnot(str: string): string =
      let schema = univ.getSchema()
      var parser = ParseState(input: str, vocab: schema)
      parser.parse("")
      case parser:
      of Ok(results: @results):
        return results.mapIt(reprHumanFriendly(univ, schema, it)).join(", ")
      of Fail(loc: @loc, message: @msg): return fmt "{loc}: {msg}"


  test "Fully-specified structures":
    check:
      structure("(doc (head (author \"Kimmy\")))")==
        "push(:doc) push(:head) push(:author) str(Kimmy) pop pop pop"
      structure("""
         (doc (head (author "Kimmy")
                    (date "2024"))
              (body (h1 (span "Hello"))))
         """)==
         "push(:doc) push(:head) push(:author) str(Kimmy) pop " &
         "push(:date) str(2024) pop pop "&
         "push(:body) push(:h1) push(:span) str(Hello) pop pop pop pop"

  test "Fully-specified structures with bare Exprs":
    check:
      structure("(doc (head \"Hello\" \"World\"))")==
        "push(:doc) push(:head) str(Hello) popi "&
        "pushi(:head) str(World) pop pop"

  test "Implicit context pushes":
    check:
      structure("doc author \"Kimmy\" h1 \"Foo\"")==
        "pushi(:doc) pushi(:head) pushi(:author) str(Kimmy) popi popi "&
        "pushi(:body) pushi(:h1) str(Foo) popi popi popi"
      structure("doc author \"Kimmy\" body span \"Foo\"")==
        "pushi(:doc) pushi(:head) pushi(:author) str(Kimmy) popi popi "&
        "pushi(:body) pushi(:span) str(Foo) popi popi popi"
      structure("doc author \"Kimmy\" body h1 span \"Foo\"")==
        "pushi(:doc) pushi(:head) pushi(:author) str(Kimmy) popi popi "&
        "pushi(:body) pushi(:h1) pushi(:span) str(Foo) popi popi popi popi"
      structure("doc author \"Kimmy\" body span h1 \"Foo\"")==
        "pushi(:doc) pushi(:head) pushi(:author) str(Kimmy) popi popi "&
        "pushi(:body) pushi(:span) popi pushi(:h1) str(Foo) "&
        "popi popi popi"
      structure("doc (author \"Kimmy\" h1 \"Foo\")")==
        "20: Couldn't unambiguously resolve symbol h1 inside author"
      structure("doc author \"Kimmy\" h1 \"Foo\")")==
        "27: Unbalanced parentheses: ')' without a '('"
      structure("(doc (head (author \"Kimmy\" (h1 \"Foo\")))")==
        "28: h1 isn't a field of author"

  test "Friendly representations":
    check:
      parseAnnot("(doc (head (author \"Kimmy\")))")==
       "(doc (head (author \"Kimmy\")))"
      parseAnnot("body span \"Foo\" \"Bar\"")==
       "(doc (body (span \"Foo\") (span \"Bar\")))"
      parseAnnot(":dup1 \"test\"")==
       "(doc (body (dup:dup1 \"test\")))"

  test "Multiple Exprs":
    check:
      parseAnnot("(doc \"Hello\") (doc \"World\")")==
       "(doc \"Hello\"), (doc \"World\")"


suite "Sqlite library":
  setup:
    var (file, name) = createTempFile("sqlite", "db")
    var lib = openSqliteLibrary name
  teardown:
    file.close()
    removeFile(Path(name))

  test "Fundamentals":
    check lib.contains(title.key)
    check lib.contains(vocab.key)

    lib.add: newDoc ":something": title "Foooo"
    let newDoc = lib.lookup ":something"
    check newDoc.isSome
    check newDoc.key == some ":something"
    check newDoc.get().allTitles == @["Foooo"]
