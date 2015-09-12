# ebnf-bff ![](https://travis-ci.org/Lokidottir/ebnf-bff.svg?branch=master)

## Parser combinators & EBNF, BFFs!

Currently barebones, but (most of) the help text is relevant (no --prune-ids yet).

### Installing

#### Installing (Linux)
As standard for installing haskell programs, you must have `ghc` and `cabal` installed

```bash
git clone https://github.com/Lokidottir/ebnf-bff
cd ebnf-bff
sudo cabal install --only-dependencies --global && sudo cabal install --global
```

#### Installing from cabal

```
sudo cabal install ebnf-bff --global
```

### Usage

**help text for `ebnf-parse`:**
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
                                     json.
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

### Todos:

* Clean up the project
* Remove dependency to Aeson, for reducing the build times.
* EBNF grammar analysis & reporting of potentially dangerous structures
  (such as parsing infinite empty strings, parsec already does this
  but we can give a source code location)
* Better error messages on failed parsing of EBNF grammar
* EBNF as defined in EBNF (properly)

### Licence

This project is under the MIT licence.
