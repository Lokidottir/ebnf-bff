import EBNF hiding (main)
import EBNF.Informal (syntax)
import EBNF.SyntaxTree
import Text.Parsec
import Data.Aeson
import Data.Aeson.Encode
import System.Environment

transforms = prune (\a -> (identifier a) == "irrelevent")

main :: IO()
main = do
    args <- getArgs
    fileContents <- readFile (args !! 0)
    case (parse syntax (args !! 0) fileContents) of
        Left err -> print err
        Right st -> do
            print (encode (transforms st))
