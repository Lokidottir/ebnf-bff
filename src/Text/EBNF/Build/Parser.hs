module Text.EBNF.Build.Parser (build, ioTryBuild, lookupGrammar) where

import Text.EBNF.SyntaxTree
import Text.EBNF.Informal (syntax)
import Text.EBNF.Helper
import Text.EBNF.Build.Parser.Parts
import Text.EBNF.Build.Parser.Except
import Text.Parsec.String
import System.IO
import Data.Either

{-|
    given a syntax tree for a valid EBNF grammar, returns a
    association list with the key as the meta identifier.
-}
build :: SyntaxTree -> [GrammarRule]
build st = rights . buildSyntax $ st


{-|
    transform that discards the information in a EBNF AST
    generated by EBNF.Informal that is not relevant.
-}
discard :: SyntaxTree -> SyntaxTree
discard = prune (\a -> identifier a `elem` list)
                where
                    list = [ "irrelevent"
                           , "concatenate symbol"
                           , "definition separator symbol"
                           , "defining symbol"
                           , "terminator symbol"]

{-|
    IO function, outputs errors if build fails.
-}
ioTryBuild :: SyntaxTree -> IO [GrammarRule]
ioTryBuild st =
    case generateReport st of
        {-
            The tree has no detectable errors, continue
            with the program as normal.
        -}
        Clean         -> return $ build st
        {-
            The tree has one or more non-critical errors,
            print these to stderr and continue with the
            building process.
        -}
        Warning warns -> do
            hPrint stderr $ Warning warns
            return $ build st
        {-
            The tree has one or more critical errors, the
            program exits with an error code.
        -}
        Failed fails  -> (die . show $ Failed fails) >> return []
