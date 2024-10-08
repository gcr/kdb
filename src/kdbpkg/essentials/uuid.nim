# kdb (c) 2024 Kimberly Wilber.
# kdb is distributed under a license that expressly prohibits military use. See "LICENSE" in this repository for details.

import random
import strformat
import strutils
import sequtils
import docs
import sugar
randomize()

## Implementation of proquints.

let
    consonants = "bdfghjklmnprstvz" ## 16 bits
    vowels = "aiou"                 ## 2 bits

    flickr_base88 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    ## about 5 bits

proc con(): string = $consonants[rand(max = consonants.high.int)]
proc vow(): string = $vowels[rand(max = vowels.high.int)]


proc proquint(): string =
    fmt"{con()}{vow()}{con()}{vow()}{con()}"

proc randomLetters(len=5): string =
    let letters = collect:
        for _ in 0..<len:
            flickr_base88[rand(max=flickr_base88.len-1)]
    result = ":" & letters.join()




proc uuid*(): ID =
    #toID(fmt":{proquint()}-{proquint()}")
    ID(randomLetters(5))

when isMainModule:
    echo uuid()
