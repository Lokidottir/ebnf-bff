module Text.EBNF.Build.Parser.Except where

import Text.Parsec.Pos
import Text.EBNF.SyntaxTree
import Text.EBNF.Helper
import Data.Function
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
    show fd = concat [failtype fd, " in " , show $ pos fd, ":\n", description fd]


data Report = Clean
            | Warning {warnings :: [FailData]}
            | Failed {failures :: [FailData]} deriving (Eq)


instance Show Report where
    show Clean       = "Clean Report"
    show (Warning w) = intercalate "\n" . map show $ w
    show (Failed f)  = intercalate "\n" . map show $ f


concatReports :: [Report] -> Report
concatReports = foldl combineReports Clean


{-|
    Combining reports is a symmetric operation where @Clean@s
    are overridden by @Warning@s and @Failed@s, whereas @Warning@s
    are overridden only by @Failed@s.
-}
combineReports :: Report -> Report -> Report
combineReports Clean Clean = Clean
combineReports Clean a = a
combineReports a Clean = combineReports Clean a
combineReports (Warning w) (Failed f) = Failed (sortBy (compare `on` pos) $ f ++ w)
combineReports (Failed f) (Warning w) = combineReports (Warning w) (Failed f)
combineReports (Failed f) (Failed f')   = Failed (sortBy (compare `on` pos) $ f ++ f')
combineReports (Warning w) (Warning w') = Warning (sortBy (compare `on` pos) $ w ++ w')


{-|
    Will analyse a syntax tree, returning reports to be combined
    together.
-}
generateReport :: SyntaxTree -> Report
generateReport st = concatReports . map (($ st) . reporter) $ reports


reports :: [SyntaxTree -> Report]
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
    | opInRepeat        = Failed [reportf]
    | hasOptionalInTail = Warning [reportw]
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
            isRepeatSeq = identifier st == "repeated sequence"
            hasOptionalInTail = any (\a -> identifier (sk a) == "optional sequence")
                                . tail' . children . head . children $ st
            isOnlyOptionals = all (\a -> identifier (sk a) == "optional sequence")
                              . children . head . children $ st
            {- skip term -> factor -> primary -}
            sk :: SyntaxTree -> SyntaxTree
            sk st' = st'

tail' :: [a] -> [a]
tail' [] = []
tail' a = tail a
