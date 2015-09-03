import Text.EBNF hiding (main)
import Text.EBNF.Informal (syntax)
import Text.EBNF.SyntaxTree
import Text.EBNF.Build.Parser
import Text.Parsec
import Data.List
import Data.Aeson
import Data.Aeson.Encode
import qualified Data.ByteString.Lazy as BS
import System.Environment
import System.IO


main :: IO()
main = getArgs >>= processArgs


{-|
    appends "=" to all elements in a list, just for differentiating
    between arguments that are followed by input.
-}
e l = map (\a -> a ++ "=") l

helpArg     =   ["-h", "--help", "-help"]
grammarArg  = e ["-g", "--grammar"]
outArgs     = e ["-o", "--output"]
primArgs    = e ["-p", "--primary-rule"]
sourceArgs  =   ["-s", "--source-files"]
formatArgs  = e ["--format"]
ebnfastArgs =   ["--export-ebnf-ast"]

{-|
    check arguments and report errors in arguments for main'
-}
processArgs :: [String] -> IO()
processArgs args
    | eleml helpArg args               =
        -- help text expected
        readFile "help.txt" >>= putStrLn
    | not . prelargs grammarArg $ args =
        -- no grammar specified
        hPutStrLn stderr "error: no grammar was provided (--grammar=<filename>)"
    | (not . prelargs primArgs $ args) &&
      (not . eleml ebnfastArgs $ args) =
        -- there is no primary rule and we're not outputting the EBNF AST
        hPutStrLn stderr "error: no primary rule provided (--primary-rule=<rule>)"
    | (not . eleml sourceArgs $ args) &&
      (not . eleml ebnfastArgs $ args) =
          -- there are no source files and we're not outputting the EBNF AST
        hPutStrLn stderr "error: no source files provided (--source-files ...)"
    | not . prelargs outArgs $ args    =
        -- add default
        processArgs ("--output=stdout":args)
    | not . prelargs formatArgs $ args =
        -- add default
        processArgs ("--format=plaintext":args)
    {-
        All arguments are present and all defaults are solved or
        provided by the user, this case is where the program is
        run
    -}
    | otherwise                        =
        main' args

{-|
    what would be main if main wasn't being used for getting the
    arguments for this function. this function does not check for
    missing or poorly formatted arguments.
-}
main' :: [String] -> IO()
main' args = putStrLn ("not yet implemented, args: " ++ (show args))

{- get the data from the given arguments -}
-- pollArgData arglist args =

{-|
    Perform elem on a list, if any element in the first list are elements
    of the second list then the function returns True, otherwise False
-}
eleml p t = or . map (\c -> elem c t) $ p

{-|
    if any of the elements of the first argument are prefixes
    of any of the elements of the second argument then the function
    returns True, otherwise false.
-}
prelargs p t = or . map (\c -> or . map (\d -> isPrefixOf c d) $ t) $ p


{-|
    removes elements from an array that whose prefixes are from a given
    list. useful for filtering arguments for recursive processing.
-}
removeprel p t = filter (\c -> not . or . map (\d -> isPrefixOf d c) $ p) $ t
