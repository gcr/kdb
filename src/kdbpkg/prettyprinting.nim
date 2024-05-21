import essentials/[docs, parsing, vocabulary, uuid, repr, vocabMacros]
import libraries/sqliteLibrary
import builtins/textual
import std/[strutils, sequtils, options, streams, times, strformat, terminal, os, tables]
import fusion/matching
import macros
import json
import deques


type
    PrettyPrinterIDKind* = enum
        ppikIdOnly,
        ppikTitleId,
        ppikTitleIfUnambiguous,
    PrettyPrinterKind* = enum
        ppkCompressed,
        ppkNewLineAlways,
    PrettyPrinter* = object
        mode = ppkNewLineAlways
        idMode = ppikTitleIfUnambiguous
        stream: Stream
        useColor = false
        vocab: Vocabulary
        lib: Library
        indentWidth = 2
        currentLevel = 0
        futureTokens: Deque[DexprToken]
