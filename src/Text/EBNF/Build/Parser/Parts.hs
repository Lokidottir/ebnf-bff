module Text.EBNF.Build.Parser.Parts (buildSyntax,
                                     buildSyntaxRule,
                                     GrammarRule,
                                     ConstructedParser) where

import Text.EBNF.SyntaxTree
import Text.Parsec.String
import Text.Parsec
import Data.List

{-
    builds a rule from syntax tree that represents a valid EBNF
    file.
-}

type GrammarRule = (Identifier, ConstructedParser)
type ConstructedParser = ([GrammarRule] -> Parser SyntaxTree)

buildSyntax :: SyntaxTree -> [GrammarRule]
buildSyntax st = map (buildSyntaxRule) (children st)

buildSyntaxRule :: SyntaxTree -> GrammarRule
buildSyntaxRule st =
    (rulename, buildDefList deflist rulename)
            where
                {- The meta identifier of the rule that is being built -}
                rulename = pollRulename st
                {- The definitions list to be built -}
                deflist =
                    maybe nulltree (find (\a -> identifier a == "definitions list") st)

{-|
    for a SyntaxTree that represents a whole rule, finds the
    first meta identifier. does not recurse into
-}
pollRulename :: SyntaxTree -> String
pollRulename st =
     content
     . maybe nulltree (find (\a -> identifier a == "meta identifier") (children st))

buildDefList :: SyntaxTree -> Identifier -> ConstructedParser
buildDefList st myid = (\a -> do
    pos <- getPosition
    ch <- choice defList
    return (SyntaxTree myid "" pos ch))
        where
            defList = map buildDef (children st)

{-|
    Alias function for buildSyntacticTerm as definition lists are
    lists of syntactic terms
-}
buildDef :: SyntaxTree -> ConstructedParser
buildDef st = buildSyntacticTerm st

buildSyntacticTerm :: SyntaxTree -> ConstructedParser
buildSyntacticTerm st = (\a -> do
    return (SyntaxTree ))
    where


buildSyntacticPrimary :: SyntaxTree -> ConstructedParser
buildSyntacticPrimary st =
    let ch = maybe nulltree find (\a -> (identifier a) /= "irrelevent") . children st
    in case (identifier ch) of
        "optional sequence" -> buildOptionalSequence ch
        "repeated sequence" -> buildRepeatedSequence ch
        "grouped sequence"  -> buildGroupedSequence ch
        "special sequence"  -> buildSpecialSequence ch
        "meta identifier"   -> buildMetaIdentifier ch
        "terminal string"   -> buildTerminalString ch
        "empty sequence"    -> buildEmptySequence ch
        otherwise           -> (\a -> nulltree)

buildOptionalSequence :: SyntaxTree -> ConstructedParser
buildOptionalSequence st =
    buildDefList . maybe nulltree find (\a -> (identifier a) /= "irrelevent") . children st
