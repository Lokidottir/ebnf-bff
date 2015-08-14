import EBNF hiding (main)
import System.Environment

main :: IO()
main = do
    args <- getArgs
    mapM putStrLn args
    putStrLn "this program is queer"
