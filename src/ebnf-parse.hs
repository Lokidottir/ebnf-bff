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
main = do
    args <- getArgs
    if (or . map (\a -> elem a args) $ ["-h", "--help", "-help"])
        then do
            {-
                print the helptext
            -}
            helptext <- readFile "help.txt"
            print helptext
        else return ()
    fileContents <- readFile (head args)
    case (parse syntax (head args) fileContents) of
        Left err -> print err
        Right st -> BS.writeFile "out.json" ((encode . discard) st)
