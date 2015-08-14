module EBNF.Structure where

import EBNF.Components
import EBNF.Helper
import EBNF.SyntaxTree
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Pos

{-

-}

-- rule: "special sequence"
specialSequenceST :: Parser SyntaxTree
specialSequenceST = do
    b <- getPosition
    specialSymbol
    a <- many (syntacticException terminalCharacter specialSymbol)
    specialSymbol
    return (SyntaxTree "special sequence" (concat a) b [])
