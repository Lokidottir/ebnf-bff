module EBNF.Helper where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

--syntacticException :: a -> b -> GenParser Char st c
syntacticException factor term = do
    notFollowedBy term
    factor

betweenSame c = between c c
