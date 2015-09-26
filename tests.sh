#!/bin/bash
cabal run -v0 -- -g=testgrammars/words/words.ebnf -p=sentence -s testgrammars/words/words.txt
cabal run -v0 -- -g=testgrammars/dangerousgrammar.ebnf -p=maybeEmpty -s
