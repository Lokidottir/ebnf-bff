module Text.EBNF.SyntaxTree where
{-
    Syntax Tree module for data type and related functions
-}
import Text.EBNF.Helper
import Text.Parsec.Pos
import Data.List
import Data.Tuple
import Data.Ord
import Data.Aeson.Types
import Data.Text (pack)

type Identifier = String
type Content = String

data SyntaxTree = SyntaxTree {
                 identifier :: !Identifier,
                 content    :: !Content,
                 position   :: !SourcePos,
                 children   :: ![SyntaxTree]
                 } deriving (Show, Eq)

instance Ord SyntaxTree where
    compare (SyntaxTree _ _ pos _) (SyntaxTree _ _ pos' _) = compare pos pos'

instance ToJSON SyntaxTree where
    toJSON (SyntaxTree i c p ch) = (object [(pack "identifier") .= i,
                                            (pack "content")    .= c,
                                            (pack "position")   .= (toJSON p),
                                            (pack "children")   .= (map (toJSON) ch)])


instance ToJSON SourcePos where
    toJSON pos = (object [(pack "name") .= (sourceName pos),
                          (pack "line") .= (sourceLine pos),
                          (pack "col")  .= (sourceColumn pos)])

{-|
    inserts a syntax tree as a child, list is sorted by source code
    position
-}
insert :: SyntaxTree -> SyntaxTree -> SyntaxTree
insert st st' = SyntaxTree (identifier st)
                           (content st)
                           (position st)
                           (insertWhere (\a -> a > st') st' (children st))

{-|
    removes any children of `st` that equal `st'`
-}
remove :: SyntaxTree -> SyntaxTree -> SyntaxTree
remove st st' = SyntaxTree (identifier st)
                           (content st)
                           (position st)
                           (filter (\a -> a /= st') (children st))

{-|
    the content of the syntax tree is merged with it's parent
    if the predicate is met.
-}
collapse :: (SyntaxTree -> Bool) -> SyntaxTree -> SyntaxTree
collapse predicate (SyntaxTree i c p ch) =
    (SyntaxTree i c' p ch')
        where
            ch' = map (collapse predicate) ch
            c' = concat (map content ch')

{-|
    prune will remove any children of `tree` that satisfy `predicate`,
    recursively
-}
prune :: (SyntaxTree -> Bool) -> SyntaxTree -> SyntaxTree
prune predicate (SyntaxTree i c p ch) =
    SyntaxTree i c p (map (prune predicate) (filter (not . predicate) ch))

{-|
    prune any children of `st` whose identifier begins with an underscore.
    might be useful for preventing syntax trees from being polluted by
    base cases of single characters by annotating the EBNF definition of
    single-char base cases such as single letters as `_letters_ = ...`
-}
pruneUnderscored :: SyntaxTree -> SyntaxTree
pruneUnderscored st = prune (\a -> ((head (identifier a)) == '_')) st

{-
pruneIdentifier :: SyntaxTree -> Identifier -> SyntaxTree
pruneIdentifier st identifier = prune (\s -> ) st
-}
isTerminal :: SyntaxTree -> Bool
isTerminal (SyntaxTree _ _ _ []) = True
isTerminal (SyntaxTree _ _ _ _)  = False
