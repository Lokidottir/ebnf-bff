import Text.EBNF hiding (main)
import Text.EBNF.Informal (syntax)
import Text.EBNF.SyntaxTree
import Text.Parsec
import Data.List
import Data.Aeson
import Data.Aeson.Encode
import qualified Data.ByteString.Lazy as BS
import System.Environment
import System.IO

transforms = prune (\a -> (identifier a) == "concatenate symbol")
           . prune (\a -> (identifier a) == "irrelevent")
           . prune (\a -> (identifier a) == "definition separator symbol")

main :: IO()
main = do
    args <- getArgs
    if (or (map (\a -> elem a args) ["-h", "--help", "-help"]))
        then do
            {-
                print the helptext
            -}
            helptext <- readFile "help.txt"
            print helptext
        else return ()
    fileContents <- readFile (args !! 0)
    case (parse syntax (args !! 0) fileContents) of
        Left err -> print err
        Right st -> BS.writeFile "out.json" ((encode . transforms) st)
