module Text.EBNF.Build.Parser.Transforms where

import Text.EBNF.SyntaxTree

idlist :: [Identifier] -> (SyntaxTree -> SyntaxTree) -> SyntaxTree -> SyntaxTree
idlist list f st
    | (identifier st) `elem` list = transform (f st)
    | otherwise                   = (SyntaxTree
                                    (identifier st)
                                    (content st)
                                    (position st)
                                    (map idlist f) (children st))
