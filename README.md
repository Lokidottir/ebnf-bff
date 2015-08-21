# ebnf-bff ![](https://travis-ci.org/Lokidottir/monadic-ebnf-bff.svg)

Parser combinators & EBNF, BFFs!

Still being developed, though `ebnf-parse <filename>` will output an AST in JSON
format to `out.json` if the file is a valid EBNF grammar.

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
    -h --help                      | show this text
    -g --grammar=filename          | load the EBNF grammar from the given file
    -o --output=[filename|stdout]  | output the AST to the given file or stdout
                                     (--output=stdout)
    --format=[json|xml|plaintext]  | the format for the AST, defaults to
                                     plaintext
    --prune-ids=[comma delim list] | removes any subtrees from the tree that
                                     have an identifier from the given list
    -s --source-files              | all arguments after this flag will be
                                     assumed to be file names for files to be
                                     parsed by the given grammar.
```
