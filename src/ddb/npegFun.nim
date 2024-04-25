import npeg

# fun!

let parser = peg "top":
  stuff <- "foo" * >"b":
    si_NP += 5
    echo "HELLO: ", $1
  top <- >stuff * >(Alpha[3]):
    echo "Then: ", $1, " and ", $2



echo parser.match("foobarbazfrob!")
