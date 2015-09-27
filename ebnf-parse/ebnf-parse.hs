import Text.EBNF hiding (main)
import Text.EBNF.Informal (syntax)
import Text.EBNF.SyntaxTree
import Text.EBNF.Helper
import Data.EBNF.Trie
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
e = map (++ "=")

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
        putStrLn helptext
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
        processArgs ("--format=json":args)
    {-
        Necessary arguments checked and defaults added, checking
        for integrity of arguments beyond this point
    -}
    | not . (`elem` formats)
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
                       . fromMaybe (length args)
                       . findIndex (`elem` sourceArgs) $ args) args
    let primaryRule = getArgData primArgs args
    let showPipe = case getArgData formatArgs args of
                        "json"    -> jsonST
                        "xml"     -> xmlST
                        otherwise -> showST
    let parserf gr fname fc =
                      case parse ((fromJust $ lookupGrammar primaryRule gr) gr) fname fc of
                          Left err -> (die . show $ err) >> return ""
                          Right st -> return $ showPipe st

    {- pure section over -}
    sourcePaths <- pollSourcePaths sourcePaths'
    grammarContent <- readFile grammarFile
    {- get grammar -}
    case parse syntax grammarFile grammarContent of
        Left err -> die . show $ err
        Right st ->
            {-
                grammar was successfully parsed, if the --export-ebnf-ast
                flag is present then we output the syntax tree of the EBNF,
                otherwise we are going to evaluate it and turn it into a
                parser then parse the source files with the parser.
            -}
            if eleml ebnfastArgs args then
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
xmlST = show

showST :: SyntaxTree -> String
showST = show

{-
    returns the full, recursed list of source files to be read
    and parsed.
-}
pollSourcePaths :: [String] -> IO [String]
pollSourcePaths paths =
    let t = tail paths
        h = head paths
    in ifM (return $ null paths) (return [])
        (ifM (doesFileExist h)
            (do
             sp <- pollSourcePaths t
             return (h:sp))
            (ifM (doesDirectoryExist h)
                (do
                    dircontent <- getDirectoryContents h
                    sp' <- pollSourcePaths dircontent
                    sp <- pollSourcePaths t
                    return (sp' ++ sp))
                (die ("error: '" ++ h ++ "' is not a file or directory") >> return [])))

output :: String -> String -> IO()
output "stdout" str = putStrLn str
output file str     = writeFile file str

{- get the data from the given arguments -}
getArgData arglist args =
    let argument = fromJust . find (\a -> any (`isPrefixOf` a) arglist) $ args
        dropnum = (1 +) . fromJust . elemIndex '=' $ argument
    in drop dropnum argument

{-
    Perform elem on a list, if or element in the first list are elements
    of the second list then the function returns True, otherwise False
-}
eleml p t = any (`elem` t) p

{-
    if or of the elements of the first argument are prefixes of or
    of the elements of the second argument then the function returns
    True, otherwise false.
-}
prelargs p t = any (\c -> any (isPrefixOf c) t) p


{-
    removes elements from an array that whose prefixes are from a given
    list. useful for filtering arguments for recursive processing.
-}
removeprel p = filter (\c -> not . any (`isPrefixOf` c) $ p)

helptext =
    unlines [
        "ebnf-parse written by fionan haralddottir, available under the MIT licence.",
        "this program is part of the ebnf-bff cabal package",
        "",
        "this is a program that parses an ISO standard EBNF grammar and outputs an",
        "abstract syntax tree in the format:",
        "",
        "identifier: <string>",
        "content: <string>",
        "position:",
        "    line: <int>",
        "    col: <int>",
        "    name: <string>",
        "children: [<syntax tree>]",
        "",
        "Use:",
        "    ebnf-parse [OPTIONS]",
        "Flags:",
        "    -h --help                      | show this text.",
        "    -p --primary-rule=rulename     | the rule to be applied to the whole of each",
        "                                     source file.",
        "    -g --grammar=filename          | load the EBNF grammar from the given file",
        "    -o --output=[filename|stdout]  | output the AST to the given file or stdout",
        "                                     (--output=stdout).",
        "    --format=[json|xml|plaintext]  | the format for the AST, defaults to",
        "                                     json.",
        "    --export-ebnf-ast              | instead of parsing given files, parse the",
        "                                     EBNF grammar and output a raw AST of the",
        "                                     grammar (still uses --prune-ids, --format",
        "                                     flags).",
        "    --prune-ids=[comma delim list] | removes any subtrees from the tree that",
        "                                     have an identifier from the given list",
        "    -s --source-files              | all arguments after this flag will be",
        "                                     assumed to be file names or directories",
        "                                     for files to be parsed by the given grammar."]
