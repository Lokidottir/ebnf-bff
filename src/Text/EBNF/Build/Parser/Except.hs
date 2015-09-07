module Text.EBNF.Build.Parser.Except where

import Text.Parsec.Pos
import Text.EBNF.SyntaxTree
import Data.List

{-
    A number of exception structures for reporting
    warnings or invalid structures in EBNF grammars
-}

data FailData = FailData {failtype :: String, description :: String, pos :: SourcePos}

instance Show FailData where
    show fd = concat [(show $ pos fd), " ", (failtype fd), ":", (description fd)]

data Report = Clean
            | Warning {warnings :: [FailData]}
            | Failed {failures :: [FailData]}

instance Show Report where
    show Clean = ""
    show (Warning w) = ""
    show (Failed f) = ""

generateReport :: SyntaxTree -> Report
generateReport st = Clean

concatReports :: [Report] -> Report
concatReports reps = foldl combineReports reps reps

combineReports :: Report -> Report -> Report
combineReports Clean Clean = Clean
combineReports Clean a = a
combineReports a Clean = a
combineReports (Warning w) (Failed f) = Failed (sortBy (\a b -> pos a < pos b) $ f ++ w)
combineReports (Failed f) (Warning w) = Failed (sortBy (\a b -> pos a < pos b) $ f ++ w)
combineReports (Failed f) (Failed f')   = Failed (sortBy (\a b -> pos a < pos b) $ f ++ f')
combineReports (Warning w) (Warning w') = Warning (sortBy (\a b -> pos a < pos b) $ w ++ w')


{-
data Report = BuildFail {
             position :: SourcePos,
             error    :: Either BuildWarning BuildError
            }

instance Show Report where
    show report = (concat . map show (errors report)) ++ (show (position report))

data BuildWarning = CircularDepends SyntaxTree
                  | OtherWarning String SyntaxTree

instance Show BuildWarning
    show warn =
-}
