module Data.EBNF.Trie where

import qualified Data.Foldable as F
import Data.Functor
import Data.List

{-|
    Type defining a Trie
-}
data Trie a = Trie  {
              value    :: a,
              children :: [Trie a]
             } deriving (Show)

{-|
    Flatten a Trie to all it's component tries
-}
flatten :: Trie a -> [Trie a]
flatten tr = tr : (concatMap flatten . children $ tr)

{-|
    Flatten a Trie to all it's values
-}
flatten' :: Trie a -> [a]
flatten' tr = value tr : (concatMap flatten' . children $ tr)

instance F.Foldable Trie where
    foldr f z = foldr f z . flatten'

instance Functor Trie where
    fmap f tr@(Trie val ch) = Trie val' ch' where
        val' = f val
        ch' = map (fmap f) ch
