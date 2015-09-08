module Text.EBNF.Helper where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Data.List
import System.IO
import System.Exit

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


{-
    die does not exist in the version of the base package used, implementation copied
    from the System.Exit source at:
    https://hackage.haskell.org/package/base-4.8.1.0/docs/src/System.Exit.html#die
-}
die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure
