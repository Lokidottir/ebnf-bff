module Text.EBNF.SyntaxTree where
{-
    Syntax Tree module for data type and related functions
-}
import Text.EBNF.Helper
import Text.Parsec.Pos
import Data.List as List
import Data.Tuple
import Data.Ord
import Data.Maybe
import Data.Aeson.Types
import Data.Text (pack)
import qualified Data.Foldable as Fold

type Identifier = String
type Content    = String

data SyntaxTree = SyntaxTree {
                 identifier :: !Identifier,
                 content    :: !Content,
                 position   :: !SourcePos,
                 children   :: ![SyntaxTree]
                 } deriving (Show, Eq)

flattenSyntaxTree :: SyntaxTree -> [SyntaxTree]
flattenSyntaxTree st = st:flattened
    where
        flattened = concatMap flattenSyntaxTree . children $ st

findST :: (SyntaxTree -> Bool) -> SyntaxTree -> Maybe SyntaxTree
findST p st | p st        = Just st
            | isJust ch   = fromJust ch
            | otherwise   = Nothing
                where
                    ch = find isJust . map (findST p) $ children st

instance Ord SyntaxTree where
    compare (SyntaxTree _ _ pos _) (SyntaxTree _ _ pos' _) = compare pos pos'

instance ToJSON SyntaxTree where
    toJSON (SyntaxTree i c p ch) = object [pack "identifier" .= i,
                                            pack "content"   .= c,
                                            pack "position"  .= toJSON p,
                                            pack "children"  .= map toJSON ch]


instance ToJSON SourcePos where
    toJSON pos = object [pack "name" .= sourceName pos,
                         pack "line" .= sourceLine pos,
                         pack "col"  .= sourceColumn pos]
{-|
    returns a syntax tree similar to the one passed but with
    the given identifier.
-}
replaceIdentifier :: Identifier -> SyntaxTree -> SyntaxTree
replaceIdentifier i st = SyntaxTree
                         i
                         (content st)
                         (position st)
                         (children st)

{-|
    returns a syntax tree similar to the one passed but with
    the given content.
-}
replaceContent :: Content -> SyntaxTree -> SyntaxTree
replaceContent c st = SyntaxTree
                      (identifier st)
                      c
                      (position st)
                      (children st)

{-|
    returns a syntax tree similar to the one passed but with
    the given position.
-}
replacePosition :: SourcePos -> SyntaxTree -> SyntaxTree
replacePosition p st = SyntaxTree
                       (identifier st)
                       (content st)
                       p
                       (children st)

{-|
    returns a syntax tree similar to the one passed but with
    the given children.
-}
replaceChildren :: [SyntaxTree] -> SyntaxTree -> SyntaxTree
replaceChildren ch st = SyntaxTree
                        (identifier st)
                        (content st)
                        (position st)
                        ch

{-|
    inserts a syntax tree as a child, list is sorted by source code
    position
-}
insert :: SyntaxTree -> SyntaxTree -> SyntaxTree
insert st st' = SyntaxTree (identifier st)
                           (content st)
                           (position st)
                           (insertWhere (> st') st' (children st))

{-|
    removes any children of `st` that equal `st'`
-}
remove :: SyntaxTree -> SyntaxTree -> SyntaxTree
remove st st' = SyntaxTree (identifier st)
                           (content st)
                           (position st)
                           (filter (/= st') (children st))

{-|
    the content of the syntax tree is merged with it's parent
    if the predicate is met.
-}
collapse :: (SyntaxTree -> Bool) -> SyntaxTree -> SyntaxTree
collapse predicate (SyntaxTree i c p ch) =
    SyntaxTree i c' p ch'
        where
            ch' = map (collapse predicate) ch
            c' = concatMap content ch'

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
pruneUnderscored = prune (\a -> head (identifier a) == '_')

isTerminal :: SyntaxTree -> Bool
isTerminal (SyntaxTree _ _ _ []) = True
isTerminal SyntaxTree{}  = False

nulltree = SyntaxTree "" "" (newPos "" 0 0) []

{-|
    For each instance of a SyntaxTree that matches a predicate,
    merge it's children with it's parent's children.
-}
raise :: (SyntaxTree -> Bool) -> SyntaxTree -> SyntaxTree
raise p st = replaceChildren (sort $ ch ++ ch') st
    where
        (a, b) = partition p (map (raise p) . children $ st)
        ch = map (raise p) b
        ch' = concatMap children a
