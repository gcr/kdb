import ../essentials/docs
import ../essentials/vocabMacros

## Docs and vocab definitions for textual data.
##
## The goal is to be able to represent markdown perfectly
## and org-mode "almost-perfectly," i.e. enough to
## represent an agenda view with complicated queries.

defTopDownBuiltinVocab:
    externalUrl ":m4mQu",
        title="external-url",
        summary="Associates this doc with some external URL, web page, or other resource."
    datePublished ":YFFWL",
        title="date-published",
        summary="Indicates the date that this doc was first made publicly available. Usually relevant for imported external docs."
    dateModified ":oH7hW",
        title="date-modified",
        summary="Indicates the date that this doc was updated. Usually relevant for imported external docs. Separate from kdb's versions system."
    author ":R1CVm",
        title="author",
        summary="Name of the author of this doc. Usually relevant for imported external docs."

defTopDownBuiltinVocab:
    textual ":cjYuE",
            title="textual",
            title="markup",
            summary="A fragment of some human-readable written text document.":
        para ":3BEuP",
                title="para",
                summary="Block paragraph containing consecutive inline text.":
            span ":G2ENB",
                    title="span",
                    summary="Single span of text.":
                spanEmph ":ywTCh", title="emph", title="italic"
                spanBold ":uwPko", title="strong", title="bold"
                spanUnderline ":LuGdV", title="underline"
                spanFootnote ":KN4YM", title="footnote"
                spanHighlight ":qnuQP", title="highlight"
                spanStrike ":d5xJG", title="strikethrough"
                spanSuperscript ":3qoSg", title="superscript"
                spanSubscript ":h5ese", title="Subscript"
                spanMath ":MUu2a", title="math", summary="Inline math in LaTeX format"
                spanTt ":rL2gp", title="tt", title="verbose", summary="Inline monotype"

        blockMath ":3ytYo",
            title="block-math", title="display-math",
            summary="Block math in LaTeX format."
        codeBlock ":w1ocE", title="code", summary="Inline code example.":
            codeBlockLang ":b8dS9", title="lang", summary="Which programming language this sample is in."
        quoteBlock ":8kN4p", title="quote", summary="Quote block":
            para

        heading ":nGo9H",
                title="heading",
                title="head",
                title="h1",
                summary="Section header":
            depth ":oiVwK",
                title="depth",
                summary="Numeric depth of this section heading. Textual documents are represented in a completely flat way, so indicating heading depth explicitly allows us to interpret structure."
            todoKeyword ":1TRGN",
                title="todo-keyword",
                title="status",
                summary="Adds an org-mode style TODO keyword to this section."
        orderedList ":kzH8E",
                    title="ordered-list",
                    title="ol",
                    summary="A numerically ordered list, like 1, 2, etc.":
            listItem ":XN2jJ",
                title="li",
                title="list-item",
                summary="Item of an ordered or unordered list":
                    para
        unorderedList ":aoJqj",
            title="unordered-list",
            title="ul",
            summary "A bulleted list.":
                listItem
        taskList ":V57gF",
            title="unordered-list",
            title="ul",
            summary="A bulleted list.":
                listItem
                listItemChecked ":bU6gD",
                    title="checked",
                    summary="Indicate that this task is complete. Used in task-list.":
                        para

        textualTable ":Wqs7n", title="table", summary="Block table.":
            tabRow ":48Lqx", title="table-row", title="tr", summary="Table row.":
                tabCell ":Xp59F", title="table-cell", title="cell", title="td", summary="Table cell.":
                    para

when isMainModule:
    import ../essentials/vocabulary
    import ../essentials/repr
    echo builtins.getFullVocabulary()

    for doc in allBuiltins():
        echo doc.key
        for expr in doc:
            echo "   ", builtins.reprHumanFriendly(builtins.getFullVocabulary(), expr)