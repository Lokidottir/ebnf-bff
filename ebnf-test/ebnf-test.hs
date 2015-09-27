import Text.EBNF hiding (main)
import Text.EBNF.Informal (syntax)
import Text.EBNF.SyntaxTree
import Text.EBNF.Helper
import Text.EBNF.Build.Parser
import Text.EBNF.Build.Parser.Except

main :: IO ()
main = print . raiseBk 

raiseBk :: SyntaxTree -> SyntaxTree
raiseBk = raise ((`elem` [
                          "definitions list",
                          "single definition",
                          "syntactic factor",
                          "syntactic primary",
                          "syntactic exception",
                          "syntactic term",
                          "integer"
                         ]) . identifier)
