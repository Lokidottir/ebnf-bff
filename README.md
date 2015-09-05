# ebnf-bff ![](https://travis-ci.org/Lokidottir/ebnf-bff.svg?branch=master)

Parser combinators & EBNF, BFFs!

Still being developed, and almost certainly bug-ridden. Currently working on
`ebnf-parse`, the commandline interface for ebnf-bff.

help text:
```
ebnf-parse written by fionan haralddottir, available under the MIT licence.
this program is part of the ebnf-bff cabal package

this is a program that parses an ISO standard EBNF grammar and outputs an
abstract syntax tree in the format:

identifier: <string>
content: <string>
position:
    line: <int>
    col: <int>
    name: <string>
children: [<syntax tree>]

Use:
    ebnf-parse [OPTIONS]
Flags:
    -h --help                      | show this text.
    -p --primary-rule=rulename     | the rule to be applied to the whole of each
                                     source file.
    -g --grammar=filename          | load the EBNF grammar from the given file
    -o --output=[filename|stdout]  | output the AST to the given file or stdout
                                     (--output=stdout).
    --format=[json|xml|plaintext]  | the format for the AST, defaults to
                                     plaintext.
    --export-ebnf-ast              | instead of parsing given files, parse the
                                     EBNF grammar and output a raw AST of the
                                     grammar (still uses --prune-ids, --format
                                     flags).
    --prune-ids=[comma delim list] | removes any subtrees from the tree that
                                     have an identifier from the given list
    -s --source-files              | all arguments after this flag will be
                                     assumed to be file names or directories
                                     for files to be parsed by the given grammar.
```
