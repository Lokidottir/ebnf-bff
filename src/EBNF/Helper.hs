module EBNF.Helper where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Data.List

--syntacticException :: a -> b -> GenParser Char st c
syntacticExceptionCombinator factor term = do
    notFollowedBy (try term)
    factor

betweenSame c = between c c

{-|

-}
insertWhere :: (a -> Bool) -> a -> [a] -> [a]
insertWhere _ element []      = [element]
insertWhere predicate element list
    | (predicate (head list)) = (element:list)
    | otherwise               = insertWhere predicate element (tail list)
