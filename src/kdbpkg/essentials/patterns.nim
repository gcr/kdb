# kdb (c) 2024 Kimberly Wilber.
# kdb is distributed under a license that expressly prohibits military use. See "LICENSE" in this repository for details.

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