module Text.EBNF.Build.Parser.Parts where

import Text.EBNF.SyntaxTree
import Text.Parsec.String
import Text.Parsec
import Data.List
import Data.Maybe


{-|
    For each instance of a SyntaxTree with the identifier @raiseIdentifier@,
    merge it's children with it's parent's children.
-}
raise :: SyntaxTree -> SyntaxTree
raise st = replaceChildren (sort $ ch ++ ch') st
    where
        parts = partition (\a -> identifier a == raiseIdentifier) (map raise . children $ st)
        ch = map raise . snd $ parts
        ch' = concatMap children . fst $ parts

{-|
    The identifier for syntax trees that have no content and need
    their children risen to the children of the syntax tree's parent.
-}
raiseIdentifier = "&raise"

{-|
    Prunes any @nulltree@s
-}
cleanup :: SyntaxTree -> SyntaxTree
cleanup = prune (== nulltree)

{-|
    Represents an EBNF grammar rule
-}
data GrammarRule = GrammarRule {
                       rulename :: String,
                       rule     :: ConstructedParser
                   }


type ConstructedParser = ([GrammarRule] -> Parser SyntaxTree)


{-|
    Null grammar rule, bad form but useful for early version.
    to be replaced by Maybe later.
-}
nullGrammar = GrammarRule "" (\_ -> return nulltree)

grToTuple :: GrammarRule -> (String, ConstructedParser)
grToTuple gr = (rulename gr, rule gr)

{-|
    lookup for grammars.
-}
lookupGrammar :: String -> [GrammarRule] -> Maybe ConstructedParser
lookupGrammar rn grs = lookup rn . map grToTuple $ grs


{-|
    Builds a rule from syntax tree that represents a valid EBNF
    file.
-}
buildSyntax :: SyntaxTree -> [Either String GrammarRule]
buildSyntax st = map buildSyntaxRule (children st)

{-|
    Builds a single syntax rule
-}
buildSyntaxRule :: SyntaxTree -> Either String GrammarRule
buildSyntaxRule st = if deflist /= nulltree then
                         Right $ GrammarRule rulename (\a -> do
                             st' <- deflistBuilt a
                             return $ cleanup . raise . replaceIdentifier rulename $ st')
                         else Left $ "error: could not find a definitions list at " ++ (show $ position st)
                             where
                                {- The meta identifier of the rule that is being built -}
                                rulename = getRulename st
                                deflistBuilt = buildDefList deflist
                                deflist = fromMaybe nulltree
                                          . find (\a -> identifier a == "definitions list")
                                          . children $ st

{-|
    For a @SyntaxTree@ that represents a whole rule, finds the
    first meta identifier. does not recurse into the tree's
    children.
-}
getRulename :: SyntaxTree -> Identifier
getRulename st =
     maybe "&failed" content
     . find (\a -> identifier a == "meta identifier")
     . children $ st

{-|
    Builds a definitions list, a list of parsers to
    try one at a time until one succeeds.
-}
buildDefList :: SyntaxTree -> ConstructedParser
buildDefList st = \a -> do
    pos <- getPosition
    let deflist' = map (\b -> b a) deflist
    ch <- choice deflist'
    return $ cleanup . raise $ SyntaxTree raiseIdentifier "" pos [ch]
        where
            deflist = map buildSingleDef
                      . filter (\a -> identifier a == "single definition")
                      . children $ st

{-
    A single definition is a concatinator seperated list ("a, b, c")
    rather than just a single parser as the name suggests, blame the
    writer for EBNF.
-}
buildSingleDef :: SyntaxTree -> ConstructedParser
buildSingleDef st = \a -> do
    pos <- getPosition
    let termlist' = map (\b -> b a) termlist
    ch <- mapM (>>= return) termlist'
    return $ SyntaxTree raiseIdentifier "" pos ch
    where
        termlist = map buildSyntacticTerm
                   . filter (\a -> identifier a == "syntactic term")
                   . children $ st

buildSyntacticTerm :: SyntaxTree -> ConstructedParser
buildSyntacticTerm st
    | isJust
      . find (\a -> identifier a == "syntactic exception")
      . children $ st = buildSTWithException st
    | otherwise       = buildSTWithoutException st

buildSTWithException :: SyntaxTree -> ConstructedParser
buildSTWithException st = \a -> do
    notFollowedBy (except a)
    factor a
        where
            except = buildSyntacticFactor
                     . fromJust
                     . find (\a -> identifier a == "syntactic exception")
                     . children $ st
            factor = buildSTWithoutException st

buildSTWithoutException :: SyntaxTree -> ConstructedParser
buildSTWithoutException st = factor
    where
        factor = buildSyntacticFactor
                 . fromJust
                 . find (\a -> identifier a == "syntactic factor")
                 . children $ st

buildSyntacticFactor :: SyntaxTree -> ConstructedParser
buildSyntacticFactor st = \a -> do
    pos <- getPosition
    ch <- count num . primary $ a
    return (SyntaxTree raiseIdentifier "" pos ch)
    where
        primary = buildSyntacticPrimary
                  . fromJust
                  . find (\a -> identifier a == "syntactic primary")
                  . children $ st
        num     = read num' :: Int
            where
                num' = case find (\a -> identifier a == "integer")
                       . children $ st of
                           Nothing -> "1"
                           Just a  -> content a

buildSyntacticPrimary :: SyntaxTree -> ConstructedParser
buildSyntacticPrimary st =
    let ch = head . children $ st
    in case identifier ch of
        "optional sequence" -> buildOptionalSequence ch
        "repeated sequence" -> buildRepeatedSequence ch
        "grouped sequence"  -> buildGroupedSequence ch
        "special sequence"  -> \_ -> return nulltree -- I /know/ it's awful
        "meta identifier"   -> buildMetaIdentifier ch
        "terminal string"   -> buildTerminalString ch
        "empty sequence"    -> \_ -> return nulltree -- I /know/ it's awful
        otherwise           -> \_ -> return nulltree -- I /know/ it's awful

{-|
    A sequence that does not have to be parsed
-}
buildOptionalSequence :: SyntaxTree -> ConstructedParser
buildOptionalSequence st =
    option nulltree . deflist
        where
            deflist =
                buildDefList
                . fromJust
                . find (\a -> identifier a == "definitions list")
                . children $ st

{-|
    A sequence that will parse 0 or more times
-}
buildRepeatedSequence :: SyntaxTree -> ConstructedParser
buildRepeatedSequence st =
    \a -> do
    pos <- getPosition
    ch <- many (deflist a)
    return $ SyntaxTree raiseIdentifier "" pos ch
        where
            deflist =
                buildDefList
                . fromJust
                . find (\a -> identifier a == "definitions list")
                . children $ st

buildGroupedSequence :: SyntaxTree -> ConstructedParser
buildGroupedSequence st = buildDefList
                          . fromJust
                          . find (\a -> identifier a == "definitions list")
                          . children $ st

buildMetaIdentifier :: SyntaxTree -> ConstructedParser
buildMetaIdentifier st = \a -> do
        let parser = fromJust . lookupGrammar iden $ a
        parser a
            where
                iden = content st

buildTerminalString :: SyntaxTree -> ConstructedParser
buildTerminalString st = \a -> do
    pos <- getPosition
    text <- string str
    return $ SyntaxTree "&string" text pos []
        where
            str = content st
