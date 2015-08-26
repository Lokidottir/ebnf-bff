module Text.EBNF.Build.Parser.Except where

import Text.Parsec.Pos
import Text.EBNF.SyntaxTree

{-
    A number of exception structures for reporting
    warnings or invalid structures
-}

data Report = BuildFail {
             position :: SourcePos,
             error    :: Either BuildWarning BuildError
            }

instance Show Report where
    show report = (concat . map show (errors report)) ++ (show (position report))

data BuildWarning = CircularDepends SyntaxTree
                  | OtherWarning String

instance Show BuildWarning
    show warn =
