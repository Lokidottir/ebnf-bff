import EBNF hiding (main)
import EBNF.Informal (syntax)
import Text.Parsec
import System.Environment

main :: IO()
main = do
    args <- getArgs
    fileContents <- readFile (args !! 0)
    case (parse syntax (args !! 0) fileContents) of
        Left err -> print err
        Right st -> print st
