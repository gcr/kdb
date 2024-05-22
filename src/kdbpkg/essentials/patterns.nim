import fusion/matching
import macros
import docs

{.experimental: "caseStmtMacros".}


macro `case`(input: Expr|Doc): untyped =
    echo "YO YO YO"
    echo input.treeRepr()
    return quote do: echo "cooool"

case topDoc:
of [1,2]: echo "hi"