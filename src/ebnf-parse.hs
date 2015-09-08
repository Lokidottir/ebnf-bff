import Text.EBNF hiding (main)
import Text.EBNF.Informal (syntax)
import Text.EBNF.SyntaxTree
import Text.EBNF.Helper
import Text.EBNF.Build.Parser
import Text.Parsec
import Data.List
import Data.Aeson
import Data.Aeson.Encode
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BSC
import System.Environment
import System.IO
import System.Exit
import System.Directory
import Control.Conditional


main :: IO()
main = getArgs >>= processArgs


{-
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
formats     =   ["json", "plaintext", "xml"]
ebnfastArgs =   ["--export-ebnf-ast"]

{-
    check arguments and report errors in arguments for main'
-}
processArgs :: [String] -> IO()
processArgs args
    | eleml helpArg args               =
        -- help text expected
        readFile "help.txt" >>= putStrLn
    | not . prelargs grammarArg $ args =
        -- no grammar specified
        die "error: no grammar was provided (--grammar|-g=<filename>)"
    | (not . prelargs primArgs $ args) &&
      (not . eleml ebnfastArgs $ args) =
        -- there is no primary rule and we're not outputting the EBNF AST
        die "error: no primary rule provided (--primary-rule|-p=<rule>)"
    | (not . eleml sourceArgs $ args) &&
      (not . eleml ebnfastArgs $ args) =
          -- there are no source files and we're not outputting the EBNF AST
        die "error: no source files provided (--source-files|-s ...)"
    | not . prelargs outArgs $ args    =
        -- add default
        processArgs ("--output=stdout":args)
    | not . prelargs formatArgs $ args =
        -- add default
        processArgs ("--format=plaintext":args)
    {-
        Necessary arguments checked and defaults added, checking
        for integrity of arguments beyond this point
    -}
    | not . (\a -> elem a formats)
          . getArgData formatArgs $ args =
             die "error: format not supported (json|plaintext|xml)"
    {-
        All arguments are present and all defaults are solved or
        provided by the user, this case is where the program is
        run
    -}
    | otherwise                        =
        main' args

{-
    what would be main if main wasn't being used for getting the
    arguments for this function. this function does not check for
    missing or poorly formatted arguments.
-}
main' :: [String] -> IO()
main' args = do
    let outputLoc = getArgData outArgs args
    let grammarFile = getArgData grammarArg args
    let sourcePaths' = drop ((1 +)
                       . maybe (length args) id
                       . findIndex (\a -> elem a sourceArgs) $ args) args
    let primaryRule = getArgData primArgs args
    let showPipe = case (getArgData formatArgs args) of
                        "json"    -> jsonST
                        "xml"     -> xmlST
                        otherwise -> showST
    let parserf = \gr fname fc ->
                      case (parse ((fromJust $ lookupGrammar primaryRule gr) gr) fname fc) of
                          Left err -> (die . show $ err) >> return ""
                          Right st -> return $ showPipe st

    {- pure section over -}
    sourcePaths <- pollSourcePaths sourcePaths'
    grammarContent <- readFile grammarFile
    {- get grammar -}
    case (parse syntax grammarFile grammarContent) of
        Left err -> die . show $ err
        Right st ->
            {-
                grammar was successfully parsed, if the --export-ebnf-ast
                flag is present then we output the syntax tree of the EBNF,
                otherwise we are going to evaluate it and turn it into a
                parser then parse the source files with the parser.
            -}
            if (eleml ebnfastArgs args) then
                output outputLoc . showPipe $ st
                else do
                    parser' <- ioTryBuild st
                    shownTrees <- mapM (\a -> readFile a >>= parserf parser' a) sourcePaths
                    output outputLoc (concat shownTrees)
                    return ()
    return ()

jsonST :: SyntaxTree -> String
jsonST st = BSC.unpack . encode $ st

xmlST :: SyntaxTree -> String
xmlST st = show st

showST :: SyntaxTree -> String
showST st = show st

{-
    returns the full, recursed list of source files to be
-}
pollSourcePaths :: [String] -> IO [String]
pollSourcePaths paths =
    let t = tail paths
        h = head paths
    in ifM (return $ paths == []) (return [])
        (ifM (doesFileExist $ h)
            (do
             sp <- pollSourcePaths t
             return (h:sp))
            (ifM (doesDirectoryExist h)
                (do
                    dircontent <- getDirectoryContents h
                    sp' <- pollSourcePaths dircontent
                    sp <- pollSourcePaths t
                    return (sp ++ sp'))
                (die ("error: '" ++ h ++ "' is not a file or directory") >> return [])))

output :: String -> String -> IO()
output file str = if (file == "stdout") then putStrLn str else writeFile file str

{- get the data from the given arguments -}
getArgData arglist args =
    let argument = fromJust . find (\a -> or . map (\b -> isPrefixOf b a) $ arglist) $ args
        dropnum = (1 +) . fromJust . findIndex (\a -> a == '=') $ argument
    in drop dropnum argument

{-
    Perform elem on a list, if any element in the first list are elements
    of the second list then the function returns True, otherwise False
-}
eleml p t = or . map (\c -> elem c t) $ p

{-
    if any of the elements of the first argument are prefixes of any
    of the elements of the second argument then the function returns
    True, otherwise false.
-}
prelargs p t = or . map (\c -> or . map (\d -> isPrefixOf c d) $ t) $ p


{-
    removes elements from an array that whose prefixes are from a given
    list. useful for filtering arguments for recursive processing.
-}
removeprel p t = filter (\c -> not . or . map (\d -> isPrefixOf d c) $ p) $ t
