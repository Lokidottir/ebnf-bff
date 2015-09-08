module Text.EBNF.Build.Parser.Except where

import Text.Parsec.Pos
import Text.EBNF.SyntaxTree
import Text.EBNF.Helper
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
    show Clean       = ""
    show (Warning w) = ""
    show (Failed f)  = ""


concatReports :: [Report] -> Report
concatReports reps = foldl combineReports (Clean) reps


{-
    Combining reports is a symmetric operation where Cleans
    are overridden by warnings and failures, whereas warnings
    are overridden only by failures. At the end the
-}
combineReports :: Report -> Report -> Report
combineReports Clean Clean = Clean
combineReports Clean a = a
combineReports a Clean = a
combineReports (Warning w) (Failed f) = Failed (sortBy (\a b -> compare (pos a) (pos b)) $ f ++ w)
combineReports (Failed f) (Warning w) = Failed (sortBy (\a b -> compare (pos a) (pos b)) $ f ++ w)
combineReports (Failed f) (Failed f')   = Failed (sortBy (\a b -> compare (pos a) (pos b)) $ f ++ f')
combineReports (Warning w) (Warning w') = Warning (sortBy (\a b -> compare (pos a) (pos b)) $ w ++ w')


{-|
    Will analyse a syntax tree, returning reports to be combined
    together
-}
generateReport :: SyntaxTree -> Report
generateReport st = concatReports $ map ($ st) reports


reports :: [(SyntaxTree -> Report)]
reports = [reportNeverTerminating]


{-|
    A never terminating parser is one that can parse an infinite
    amount of empty strings, such parsers can be achieved with
    @{[identifer]}@ pattern rules, which can parse indefinitely
    but never terminate.
-}
reportNeverTerminating :: SyntaxTree -> Report
reportNeverTerminating st =
    let rep = (\_ -> Clean) st
        reps = map reportNeverTerminating . children $ st
    in concatReports $ rep:reps
