import random
import strformat
import docs
randomize()

## Implementation of proquints.

let
    consonants = "bdfghjklmnprstvz" ## 16 bits
    vowels = "aiou" ## 2 bits

proc con():string = $consonants[rand(max=consonants.high.int)]
proc vow():string = $vowels[rand(max=vowels.high.int)]


proc proquint(): string =
    fmt"{con()}{vow()}{con()}{vow()}{con()}"
proc uuid*(): ID =
    toID(fmt":{proquint()}-{proquint()}")

when isMainModule:
    echo uuid()