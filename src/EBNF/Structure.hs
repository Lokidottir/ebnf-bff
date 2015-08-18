module EBNF.Structure where

import EBNF.Components
import EBNF.Helper
import EBNF.SyntaxTree
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Pos

{-

-}

syntax :: Parser SyntaxTree
syntax = do
    pos <- getPosition
    children <- many1 syntaxRule
    return (SyntaxTree "syntax" "" pos children)

syntaxRule :: Parser SyntaxTree
syntaxRule = do
    pos <- getPosition
    children <- do
        iden <- metaIdentifier
        def <- defineSymbol
        defList <- definitionsList
        term <- terminalSymbol
        return [iden, def, defList, term]
    return (SyntaxTree "syntax rule" "" pos children)

definitionsList :: Parser SyntaxTree
definitionsList = do
    pos <- getPosition
    children <- do
        sDef <- singleDefinition
        defList <- many (do
            sep <- alternationSymbol
            nDef <- singleDefinition
            return [sep, nDef])
        return (sDef:(concat defList))
    return (SyntaxTree "definitions list" "" pos children)

singleDefinition :: Parser SyntaxTree
singleDefinition = do
    pos <- getPosition
    term <- syntacticTerm
    termList <- many (do
        con <- concatSymbol
        term' <- syntacticTerm
        return [con, term'])
    return (SyntaxTree "single definition" "" pos (term:(concat termList)))

syntacticTerm :: Parser SyntaxTree
syntacticTerm = do
    pos <- getPosition
    factor <- syntacticFactor
    except <- option [] (do
        sym <- exceptSymbol
        exceptedRule <- syntacticException
        return [sym, exceptedRule])
    return (SyntaxTree "syntactic term" "" pos (factor:except))

{-
    the definition of a syntactic exception isn't formally
    defined, but usage suggests it's like a syntactic factor
-}
syntacticException :: Parser SyntaxTree
syntacticException = syntacticFactor

syntacticFactor :: Parser SyntaxTree
syntacticFactor = 
