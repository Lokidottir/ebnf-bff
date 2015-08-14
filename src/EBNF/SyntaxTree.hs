module EBNF.SyntaxTree where
{-
    Syntax Tree module for data type and related functions
-}
import Text.Parsec.Pos

data SyntaxTree = SyntaxTree {
                 identifier :: String,
                 content    :: String,
                 position   :: SourcePos,
                 children   :: [SyntaxTree]
                 } deriving (Show, Eq, Ord)

append :: SyntaxTree -> SyntaxTree -> SyntaxTree
append st st' = SyntaxTree (identifier st)
                           (content st)
                           (position st)
                           ((children st) ++ [st'])

remove :: SyntaxTree -> SyntaxTree -> SyntaxTree
remove st st' = SyntaxTree (identifier st)
                           (content st)
                           (position st)
                           (filter (\a -> a /= st') (children st))

{-
    prune will remove any children of `tree` that satisfy `predicate`
    recursively
-}
prune :: (SyntaxTree -> Bool) -> SyntaxTree -> SyntaxTree
prune predicate tree = SyntaxTree (identifier tree)
                                  (content tree)
                                  (position tree)
                                  (map (prune predicate) (filter (notF predicate) (children tree)))

notF :: (a -> Bool) -> a -> Bool
notF f a = not (f a)

{-
    pruneUnderscored will prune any children of `st` whose identifier
    begins with an underscore. might be useful for preventing syntax
    trees from being polluted by base cases of single characters.
-}
pruneUnderscored :: SyntaxTree -> SyntaxTree
pruneUnderscored st = prune (\a -> (head (identifier a) == '_')) st
