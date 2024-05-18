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
import tables

suite "Bulit-in refs":
  test "Essential ref operations":
    check:
      vocabFor.has title
      vocabFor.hasEqual title, "vocab-for"
      not vocabFor.hasEqual(title, "something else")
      vocabFor.allTitles.toSeq == @["vocab-for"]
      vocabFor.firstTitle == some "vocab-for"

  test "Check titles of child Exprs":
    let docs: seq[Doc] = vocabFor.items.toSeq.mapIt(
      builtins.lookup(it.kind).get())
    let titles = docs.mapIt(it.firstTitle)
    check titles == @[some "vocab-for", some "title", some "summary"]

  test "Multiple children":
    let x = newDoc ID":multichild":
      title "one"
      title "two"
    check x.firstTitle == some "one"
    check (x / title).toSeq.mapIt(it.val) == @["one", "two"]
    check x.has title
    check not x.has vocabFor
    check x.hasEqual(title, "one")
    check x.hasEqual(title, "two")
    check x.allTitles.toSeq == @["one", "two"]

  test "Deeply nested refs":
    let
      head = newDoc ID":head": vocabFor ":top"
      author = newDoc ID":auth": vocabFor ":head"
      date = newDoc ID":date": vocabFor ":head"
      someDoc = newDoc ID":someDoc":
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
    check builtins.contains vocabFor.key
    check builtins.contains title.key

  test "Making ephemeral refs doesn't touch builtins":
    let x = newDoc ID":abc": title "something"
    check:
      @["something"] == x.allTitles.toSeq
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
    var univ = newMapLibrary()
    univ.add: newDoc ID":travel":
      title "Travel"
      vocabFor ":top"
    univ.add: newDoc ID":dd":
      title "Doc"
      vocabFor ":top"
    univ.add: newDoc ID":uuidHead":
      title "Head"
      vocabFor ":dd"
      vocabHas ":category"
      vocabHas ":someDup2"
      vocabHas ":undefined-lololol"
    univ.add: newDoc ID":uuidDate":
      title "Date"
      vocabFor ":uuidHead"
    univ.add: newDoc ID":substringPrecedenceMatch":
      title "Cats"
      vocabFor ":uuidHead"
    univ.add: newDoc ID":titleButInsideHead":
      title "Title"
      vocabFor ":uuidHead"
    univ.add: newDoc ID":someDup1":
      title "DuplicateName"
      vocabFor ":uuidHead"
    univ.add: newDoc ID":someDup2":
      title "DuplicateName"
      title "UnambiguousName"
    univ.add: newDoc ID":someDup3":
      title "NameMatchMethodTest"
      vocabFor ":uuidHead"
    univ.add: newDoc ID":someDup4":
      title "name_match_method_test"
      vocabFor ":uuidHead"
    univ.add: newDoc ID":category":
      title "category"
      title "A descendant of head"
    univ.add: newDoc ID":someDup5":
      title "Vocab-same-as testing"
      vocabSameAs ":uuidHead"
      vocabHas ":dd"

  test "Unpolluted builtins":
    check ID":dd" in univ
    check ID":dd" notin builtins
    check ID":category" in univ

  test "ID parsing":
    check "foo".toTitleId == TitleId(title: "foo", id: ID(""))
    check "foo:".toTitleId == TitleId(title: "foo", id: ID(""))
    check ":abcd".toTitleId == TitleId(title: "", id: ID":abcd")
    check "a:b".toTitleId == TitleId(title: "a", id: ID":b")
    check ":".toTitleId == TitleId(title: "", id: ID(""))

  test "Direct resolution":
    let vocab = univ.getFullVocabulary()
    check:
      vocab.resolveDirectly("Doc", context=ID":top").isSome
      vocab.resolveDirectly("NoMatch", context=ID":top").isNone
      vocab.resolveDirectly("Title", context=ID":uuidHead").key == some ID":titleButInsideHead"
      vocab.resolveDirectly("Title", context=ID":top").key == some title.key
      vocab.resolveDirectly("Date", context=ID":top").key == none(ID)
      vocab.resolveDirectly("Date", context=ID":uuidHead").key == some ID":uuidDate"
      # substring
      vocab.resolveDirectly("Dat", context=ID":uuidHead").key == some ID":uuidDate"
      # both substring and fuzzy match return different modes, but substring is less ambiguous so it should be preferred
      vocab.resolveDirectly("ats", context=ID":uuidHead").key == some ID":substringPrecedenceMatch"
      # fuzzzy
      vocab.resolveDirectly("catgy", context=ID":uuidHead").key == some ID":category"
      vocab.resolveDirectly("D", context=ID":uuidHead").key == none(ID)
      vocab.resolveDirectly("as", context=ID":uuidHead").key == none(ID)
      vocab.resolveDirectly("DuplicateName", context=ID":uuidHead").isNone
      vocab.resolveDirectly(":someDup2", context=ID":uuidHead").key == some ID":someDup2"
      vocab.resolveDirectly("NONMATCHING:someDup2", context=ID":uuidHead").isNone
      vocab.resolveDirectly("DuplicateName:someDup1", context=ID":uuidHead").key == some ID":someDup1"
      vocab.resolveDirectly("$:omeDup2", context=ID":top").isNone
      vocab.resolveDirectly("UnambiguousName", context=ID":uuidHead").key == some ID":someDup2"
      vocab.resolveDirectly("unambiguous-name", context=ID":uuidHead").key == some ID":someDup2"
      vocab.resolveDirectly("UNAMBIGUOUS_NAME", context=ID":uuidHead").key == some ID":someDup2"
      vocab.resolveDirectly("category", context=ID":uuidHead").key == some ID":category"

  test "Resolution order should matter":
    let vocab = univ.getFullVocabulary()
    check:
      vocab.resolveDirectly("name-matchMethod_test", context=ID":uuidHead").isNone
      vocab.resolveDirectly("name_match_method_test", context=ID":uuidHead").key == some ID":someDup4"
      vocab.resolveDirectly("NameMatchMethodTest", context=ID":uuidHead").key == some ID":someDup3"
      vocab.resolveDirectly("nameMatchMethodTest", context=ID":uuidHead").isNone
  test "Indirect resolution":
    univ.add: newDoc ID":recDup":
      title "RecursiveDuplicate"
      vocabFor ":travel"
      vocabFor ":dd"
      vocabFor ":uuidHead"
      vocabFor ":recDup"
    let vocab = univ.getFullVocabulary()
    proc resolveIndirectly(title: string, context: ID): Option[seq[ID]] =
      if Some(@path) ?= vocab.resolveIndirectly(title, context):
        return some path.mapIt(it.key)
    check:
      resolveIndirectly("name-matchMethod_test", context=ID":top").isNone
      resolveIndirectly("name-matchMethod_test", context=ID":dd").isNone
      resolveIndirectly("unambiguousName", context=ID":top") == some @[ID":dd", ID":uuidHead", ID":someDup2"]
      resolveIndirectly("unambiguous-name", context=ID":top") == some @[ID":dd", ID":uuidHead", ID":someDup2"]
      resolveIndirectly("unambiguousName", context=ID":dd") == some @[ID":uuidHead", ID":someDup2"]
      resolveIndirectly("unambiguousName", context=ID":travel").isNone

      # only one node appearing in many contexts
      vocab.resolveDirectly("RecursiveDuplicate", context=ID":top").isNone
      resolveIndirectly("RecursiveDuplicate", context=ID":top").isNone
      vocab.resolveDirectly("RecursiveDuplicate", context=ID":dd").key == some ID":recDup"
      resolveIndirectly("RecursiveDuplicate", context=ID":dd").isNone
      vocab.resolveDirectly("RecursiveDuplicate", context=ID":travel").key == some ID":recDup"
      resolveIndirectly("RecursiveDuplicate", context=ID":travel").isNone
      resolveIndirectly("RecursiveDuplicate", context=ID":uuidHead").isNone
      vocab.resolveDirectly("RecursiveDuplicate", context=ID":uuidHead").key == some ID":recDup"
      resolveIndirectly("RecursiveDuplicate", context=ID":recDup").isNone
      vocab.resolveDirectly("RecursiveDuplicate", context=ID":recDup").key == some ID":recDup"

      # vocabHas and vocabFor
      resolveIndirectly("category", context=ID":top") == some @[ID":dd", ID":uuidHead", ID":category"]

  test "Recursive top resolution":
    univ.add: newDoc ID":meh-meh":
      vocabHas ":top"
      vocabFor ":uuidHead"
      title "recurse"
    let vocab = univ.getFullVocabulary()
    check:
      vocab.resolveDirectly("Doc", context=ID":top").isSome
      vocab.resolveDirectly("NoMatch", context=ID":top").isNone
      vocab.resolveDirectly("Title", context=ID":uuidHead").key == some ID":titleButInsideHead"
      vocab.resolveDirectly("Title", context=ID":top").key == some title.key
      vocab.resolveDirectly("recurse", context=ID":uuidHead").key == some ID":meh-meh"
      # this is the kicker: don't structuralize past Top
      # otherwise this might resolve to
      # head->recurse->top->doc->head->recurse...
      vocab.resolveIndirectly("Doc", context=ID":uuidHead").isNone

  test "Vocab-same-as resolution":
    let vocab = univ.getFullVocabulary()
    check:
      vocab.resolveDirectly("category", context=ID":someDup5").key == some ID":category"
      vocab.resolveDirectly("Doc", context=ID":someDup5").key == some ID":dd"

suite "Structuralization":
  setup:
    var univ = newMapLibrary()
    univ.add: newDoc ID":doc": title "doc"; vocabFor ":top"
    univ.add: newDoc ID":head": title "head"; vocabFor ":doc"
    univ.add: newDoc ID":author": title "author"; vocabFor ":head"
    univ.add: newDoc ID":date": title "date"; vocabFor ":head"
    univ.add: newDoc ID":body": title "body"; vocabFor ":doc"
    univ.add: newDoc ID":h1": title "h1"; vocabFor ":body"
    univ.add: newDoc ID":span": title "span"; vocabFor ":h1"; vocabFor ":body"
    univ.add: newDoc ID":dup1": title "dup"; vocabFor ":body"; vocabFor ":body"
    univ.add: newDoc ID":dup2": title "dup"; title "dup2"; vocabFor ":body"; vocabFor ":body"
    proc structure(str: string): string =
      var parser = ParseState(input: str, vocab: univ.getFullVocabulary())
      parser.processTokenStream()
      parser.structuralize(ID":top")
      case parser:
      of Ok(tokens: @tokens): return tokens.mapIt($it).join(" ")
      of Fail(loc: @loc, message: @msg): return fmt "{loc}: {msg}"
    proc parseAnnot(str: string): string =
      let vocab = univ.getFullVocabulary()
      var parser = ParseState(input: str, vocab: vocab)
      parser.parse()
      case parser:
      of Ok(results: @results):
        return results.mapIt(reprHumanFriendly(univ, vocab, it)).join(", ")
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
        "20: Couldn't unambiguously resolve symbol h1 inside :author"
      structure("doc author \"Kimmy\" h1 \"Foo\")")==
        "27: Unbalanced parentheses: ')' without a '('"
      structure("(doc (head (author \"Kimmy\" (h1 \"Foo\")))")==
        "28: Couldn't unambiguously resolve symbol h1 inside :author"
      structure("title \"Foo\" title \"Bar\"")==
        "pushi(:qyQgm) str(Foo) popi pushi(:qyQgm) str(Bar) popi"
      structure("title \"Foo\" (title \"Bar\")")==
        "pushi(:qyQgm) str(Foo) popi push(:qyQgm) str(Bar) pop"
      structure("title \"Foo\" author title \"Bar\"")==
        "pushi(:qyQgm) str(Foo) popi pushi(:doc) pushi(:head) pushi(:author) popi popi popi pushi(:qyQgm) str(Bar) popi"
      structure("author author author ")==
        "pushi(:doc) pushi(:head) pushi(:author) popi pushi(:author) popi pushi(:author) popi popi popi"
      structure("author (author (author)) ")==
        "16: Couldn't unambiguously resolve symbol author inside :author"
      structure("head author \"Foo\" head author \"Bar\" author \"Baz\" head head")==
        "pushi(:doc) pushi(:head) pushi(:author) str(Foo) popi popi pushi(:head) pushi(:author) str(Bar) popi pushi(:author) str(Baz) popi popi pushi(:head) popi pushi(:head) popi popi"

  test "Friendly representations":
    check:
      parseAnnot("(doc (head (author \"Kimmy\")))")==
       "(doc (head (author \"Kimmy\")))"
      parseAnnot("body span \"Foo\" \"Bar\"")==
       "(doc (body (span \"Foo\") (span \"Bar\")))"
      parseAnnot(":dup1 \"test\"")==
       "(doc (body (dup:dup1 \"test\")))"
    univ.add: newDoc ID":a": title "Space Here"; vocabFor ":top"
    check:
      parseAnnot(":a \"test\"")==
       "(:a \"test\")"

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
    check lib.contains(vocabFor.key)

    lib.add: newDoc ID":something": title "Foooo"
    let newDoc = lib.lookup ID":something"
    check newDoc.isSome
    check newDoc.key == some ID":something"
    check newDoc.get().allTitles.toSeq == @["Foooo"]

  test "Broken vocab":
    check: "TODO: please add tests to ensure broken vocab's handled properly" == ""
  #test "Should never add broken vocab":
  #  expect ValueError:
  #    lib.add: newDoc ID":broken": title "Broken"; vocabFor "nope"
  #  check lib.lookup(ID":broken").isNone