module Text.EBNF.Build.Parser.Except (FailData,
                                      Report,
                                      generateReport,
                                      ) where

import Text.Parsec.Pos
import Text.EBNF.SyntaxTree
import Text.EBNF.Helper
import Data.List

{-
    A number of exception structures for reporting
    warnings or invalid structures in EBNF grammars
-}

data FailData = FailData {
                    failtype :: String,
                    description :: String,
                    pos :: SourcePos
                    } deriving (Eq)


instance Show FailData where
    show fd = concat [(failtype fd), " in " ,(show $ pos fd), ":\n", (description fd)]


data Report = Clean
            | Warning {warnings :: [FailData]}
            | Failed {failures :: [FailData]} deriving (Eq)


instance Show Report where
    show Clean       = "Clean Report"
    show (Warning w) = concat . intersperse "\n" . map show $ w
    show (Failed f)  = concat . intersperse "\n" . map show $ f


concatReports :: [Report] -> Report
concatReports reps = foldl combineReports (Clean) reps


{-|
    Combining reports is a symmetric operation where @Clean@s
    are overridden by @Warning@s and @Failed@s, whereas @Warning@s
    are overridden only by @Failed@s.
-}
combineReports :: Report -> Report -> Report
combineReports Clean Clean = Clean
combineReports Clean a = a
combineReports a Clean = combineReports Clean a
combineReports (Warning w) (Failed f) = Failed (sortBy (\a b -> compare (pos a) (pos b)) $ f ++ w)
combineReports (Failed f) (Warning w) = combineReports (Warning w) (Failed f)
combineReports (Failed f) (Failed f')   = Failed (sortBy (\a b -> compare (pos a) (pos b)) $ f ++ f')
combineReports (Warning w) (Warning w') = Warning (sortBy (\a b -> compare (pos a) (pos b)) $ w ++ w')


{-|
    Will analyse a syntax tree, returning reports to be combined
    together.
-}
generateReport :: SyntaxTree -> Report
generateReport st = concatReports . map ($ st) . map reporter $ reports


reports :: [(SyntaxTree -> Report)]
reports = [neverTerminating]

reporter :: (SyntaxTree -> Report) -> SyntaxTree -> Report
reporter fn st = let rep = fn st
                     reps = map fn $ children st
                 in concatReports (rep:reps)

{-|
    @Failed@ when a repeat sequence with only optional sequences or has
    an optional sequence as it's first subsequence

    @Warning@ when a repeat sequence contains a definitions list that
    contains an optional sequence.

    @Clean@ otherwise
-}
neverTerminating :: SyntaxTree -> Report
neverTerminating (SyntaxTree _ _ _ []) = Clean
neverTerminating st
    | opInRepeat st        = Failed [reportf]
    | hasOptionalInTail st = Warning [reportw]
    | otherwise            = Clean
        where
            reportf =
                FailData "failure"
                         "sequence will never terminate (repeat sequence only contains or favours optionals)"
                         (position st)
            reportw =
                FailData "warning"
                         "sequence may never terminate (contains optional sequence in a repeat sequence)"
                         (position st)
            opInRepeat = (isRepeatSeq && isOnlyOptionals) || opIsFirstInDef
            opIsFirstInDef = isRepeatSeq && ((identifier . head . children . head $ children st) == "optional sequence")
            {- Optional sequence is in a repeat sequence -}
            isRepeatSeq = (identifier st) == "repeated sequence"
            hasOptionalInTail = or
                                . map (\a -> identifier (sk a) == "optional sequence")
                                . tail' . children . head . children $ st
            isOnlyOptionals = and
                              . map (\a -> identifier (sk a) == "optional sequence")
                              . children . head . children $ st
            {- skip term -> factor -> primary -}
            sk :: SyntaxTree -> SyntaxTree
            sk st' = head . children . head . children . head . children $ st'

tail' :: [a] -> [a]
tail' [] = []
tail' a = tail a
