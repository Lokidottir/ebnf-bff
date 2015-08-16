module EBNF.Helper where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Data.List

--syntacticException :: a -> b -> GenParser Char st c
syntacticException factor term = do
    notFollowedBy term
    factor

betweenSame c = between c c

{-|

-}
insertWhere :: (a -> Bool) -> a -> [a] -> [a]
insertWhere _ element []              = [element]
insertWhere predicate element list    =
    (fst part) ++ [element] ++ (snd part)
        where part = partition predicate list
