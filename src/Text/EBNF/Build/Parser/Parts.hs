module Text.EBNF.Build.Parser.Parts (buildSyntax,
                                     buildSyntaxRule,
                                     GrammarRule,
                                     ConstructedParser) where

import Text.EBNF.SyntaxTree
import Text.Parsec.String
import Text.Parsec
import Data.List
import Data.Maybe



{-|
    For each instance of a SyntaxTree with the identifier raiseIdentifier
    merge it's children with it's parent's children.
-}
raise :: SyntaxTree -> SyntaxTree
raise st = replaceChildren (sort $ ch ++ ch') st
    where
        parts = partition (\a -> (identifier a) == raiseIdentifier) (map raise . children $ st)
        ch = map raise . snd $ parts
        ch' = concat . map children . fst $ parts

{-|
    The identifier for syntax trees that have no content and need
    their children risen to the children of the syntax tree's parent.
-}
raiseIdentifier = "&raise"

cleanup :: SyntaxTree -> SyntaxTree
cleanup st = prune (\a -> a == nulltree) st

newtype GrammarRule = GrammarRule (String, ConstructedParser)

type ConstructedParser = ([GrammarRule] -> Parser SyntaxTree)

{-
    builds a rule from syntax tree that represents a valid EBNF
    file. raise . cleanup . replaceIdentifier rulename .
-}
buildSyntax :: SyntaxTree -> [GrammarRule]
buildSyntax st = map (buildSyntaxRule) (children st)

buildSyntaxRule :: SyntaxTree -> GrammarRule
buildSyntaxRule st = GrammarRule (rulename, (buildDefList deflist))
                        where
                            {- The meta identifier of the rule that is being built -}
                            rulename = pollRulename st
                            {- The definitions list to be built -}
                            deflist = fromJust
                                      . find (\a -> (identifier a) == "definitions list")
                                      . children $ st

{-|
    for a SyntaxTree that represents a whole rule, finds the
    first meta identifier. does not recurse into
-}
pollRulename :: SyntaxTree -> Identifier
pollRulename st =
     content
     . fromJust
     . find (\a -> (identifier a) == "meta identifier")
     . children $ st

buildDefList :: SyntaxTree -> ConstructedParser
buildDefList st = (\a -> do
    pos <- getPosition
    let deflist' = map (\b -> b a) deflist
    ch <- choice (deflist a)
    return (SyntaxTree raiseIdentifier "" pos ch))
        where
            deflist = map buildSingleDef
                      . filter (\a -> (identifier a) == "single definition")
                      . children $ st

{-
    A single definition is a concatinator seperated list ("a, b, c")
    rather than just a single parser as the name suggests, blame the
    writer for EBNF.
-}
buildSingleDef :: SyntaxTree -> ConstructedParser
buildSingleDef st = (\a -> do
    pos <- getPosition
    ch <- mapM (\b -> b a) termlist
    return (SyntaxTree raiseIdentifier "" pos ch))
    where
        termlist = map buildSyntacticTerm
                   . filter (\a -> identifier == "syntactic term")
                   . children $ st

buildSyntacticTerm :: SyntaxTree -> ConstructedParser
buildSyntacticTerm st
    | isJust
      . find (\a -> (identifier a) == "syntactic exception")
      . children $ st = buildSTWithException st
    | otherwise       = buildSTWithoutException st

buildSTWithException :: SyntaxTree -> ConstructedParser
buildSTWithException st = (\a -> do
    notFollowedBy (except a)
    return (factor a))
        where
            except = buildSyntacticFactor
                     . fromJust
                     . find (\a -> (identifier a) == "syntactic exception") $ st
            factor = buildSTWithoutException st

buildSTWithoutException :: SyntaxTree -> ConstructedParser
buildSTWithoutException st = (\a -> return factor a)
    where
        factor = buildSyntacticFactor
                 . fromJust
                 . find (\a -> (identifier a) == "syntactic factor")
                 . children $ st

buildSyntacticFactor :: SyntaxTree -> ConstructedParser
buildSyntacticFactor st = (\a -> count num . primary $ a)
    where
        primary = buildSyntacticPrimary
                  . fromJust
                  . find (\a -> identifier $ a == "syntactic primary")
                  . children $ st
        num     = read (maybe "1"
                       . find (\a -> identifier $ a == "integer")
                       . children $ st) :: Int

buildSyntacticPrimary :: SyntaxTree -> ConstructedParser
buildSyntacticPrimary st =
    let ch = head . children st
    in case (identifier ch) of
        "optional sequence" -> buildOptionalSequence ch
        "repeated sequence" -> buildRepeatedSequence ch
        "grouped sequence"  -> buildGroupedSequence ch
        "special sequence"  -> (\a -> nulltree)
        "meta identifier"   -> buildMetaIdentifier ch
        "terminal string"   -> buildTerminalString ch
        "empty sequence"    -> (\a -> nulltree)
        otherwise           -> (\a -> nulltree)

{-|
    A sequence that does not have to be parsed
-}
buildOptionalSequence :: SyntaxTree -> ConstructedParser
buildOptionalSequence st =
    (\a -> option nulltree (deflist a))
        where
            deflist =
                buildDefList
                . fromJust find (\a -> identifier $ a == "definitions list")
                . children $ st

buildRepeatedSequence :: SyntaxTree -> ConstructedParser
buildRepeatedSequence st =
    (\a -> do
    pos <- getPosition
    ch <- many deflist
    return (SyntaxTree raiseIdentifier "" pos ch))
        where
            deflist =
                buildDefList
                . fromJust
                . find (\a -> identifier $ a == "definitions list")
                . children $ st

buildGroupedSequence :: SyntaxTree -> ConstructedParser
buildGroupedSequence st = buildDefList
                          . fromJust
                          . find (\a -> identifier $ a == "definitions list")
                          . children $ st

buildMetaIdentifier :: SyntaxTree -> ConstructedParser
buildMetaIdentifier st =
    (\a -> lookup iden a)
        where
            iden = content st

buildTerminalString :: SyntaxTree -> ConstructedParser
buildTerminalString st = (\a -> do
    pos <- getPosition
    text <- string str
    return (SyntaxTree ))
        where
            str = (content st)
