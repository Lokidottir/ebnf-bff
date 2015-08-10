module EBNF.Components where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Expr

{-
vowel :: GenParser Char st Char
vowel  = oneOf "aeiou"
-}

-- rule: "concatenate symbol"
concatSymbol :: GenParser Char st Char
concatSymbol = char ','

-- rule: "defining symbol"
defineSymbol :: GenParser Char st Char
defineSymbol = char '='

-- rule: "definition separator symbol"
alternationList :: GenParser Char st Char
alternationList = char '|' <|> char '/' <|> char '!'
