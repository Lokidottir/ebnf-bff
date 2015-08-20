import EBNF hiding (main)
import EBNF.Informal (syntax)
import EBNF.SyntaxTree
import Text.Parsec
import Data.Aeson
import Data.Aeson.Encode
import qualified Data.ByteString.Lazy as BS
import System.Environment

transforms = prune (\a -> (identifier a) == "concatenate symbol")
           . prune (\a -> (identifier a) == "irrelevent")
           . prune (\a -> (identifier a) == "definition separator symbol")

main :: IO()
main = do
    args <- getArgs
    fileContents <- readFile (args !! 0)
    case (parse syntax (args !! 0) fileContents) of
        Left err -> print err
        Right st -> BS.writeFile "out.json" ((encode . transforms) st)
