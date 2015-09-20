module Text.EBNF.Informal where

import Text.EBNF.Helper
import Text.EBNF.SyntaxTree
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Data.List
import Data.Maybe
{-
    An implementation of an EBNF parser from the ISO EBNF informal
    definitions.

    TODO: better error messages
-}

{-
    Implementation of MissingH's stripr function to lower the number
    of dependencies.
-}
strip :: String -> String
strip str = reverse . strip' . reverse $ str

strip' :: String -> String
strip' str
    | str == "" = ""
    | not . (`elem` stripWSList) . head $ str = str
    | otherwise = strip' . tail $ str

stripWSList = " \t\n\v\f"

primST :: Parser String -> String -> Parser SyntaxTree
primST par name = do
    pos <- getPosition
    text <- par
    return (SyntaxTree name text pos [])

{-
    TODO: parsers for less verbose sourse code
        primChild    | parser that will parse for a single child
        primChildren | parser that will parse for many children
        primTerminal | parser that will parse for a string (primST)
-}


{-|
    Syntax parser, parses an entire syntax
-}
syntax :: Parser SyntaxTree
syntax = do
    pos <- getPosition
    ch <- many1 syntaxRule
    return (SyntaxTree "syntax" "" pos ch)

{-|
    Syntax rule parser, parses a single syntax rule
-}
syntaxRule :: Parser SyntaxTree
syntaxRule = do
    pos <- getPosition
    ch <- do
        blPre <- irrelevent
        meta <- metaIdentifier
        blA <- irrelevent
        eq <- primST (string "=") "defining symbol"
        blB <- irrelevent
        defL <- definitionsList
        blC <- irrelevent
        ter <- primST (string ";" <|> string ".") "terminator symbol"
        blPost <- irrelevent
        return [blPre, meta, blA, eq, blB, defL, blC, ter, blPost]
    return (SyntaxTree "syntax rule" "" pos ch)

definitionsList :: Parser SyntaxTree
definitionsList = do
    pos <- getPosition
    defA <- singleDefinition
    list <- many $ do
        primST (string "|" <|> string "!" <|> string "/") "definition separator symbol"
        singleDefinition
    return $ SyntaxTree "definitions list" "" pos (defA:list)

singleDefinition :: Parser SyntaxTree
singleDefinition = do
    pos <- getPosition
    blPre <- irrelevent
    termA <- syntacticTerm
    list <- many (do
        blInListA <- irrelevent
        concatSym <- primST (string ",") "concatenate symbol"
        blInListB <- irrelevent
        termInList <- syntacticTerm
        return [blInListA, concatSym, blInListB, termInList])
    blPost <- irrelevent
    return (SyntaxTree "single definition" "" pos ([blPre, termA] ++ concat list ++ [blPost]))

syntacticTerm :: Parser SyntaxTree
syntacticTerm = do
    pos <- getPosition
    blPre <- irrelevent
    factor <- syntacticFactor
    exceptBl <- option [] (do
        blInListA <- irrelevent
        exceptSym <- primST (string "-") "except symbol"
        blInListB <- irrelevent
        exception <- syntacticException
        return [blInListA, exceptSym, blInListB, exception]
        )
    blPost <- irrelevent
    return (SyntaxTree "syntactic term" "" pos ([blPre, factor] ++ exceptBl ++ [blPost]))

{-|
    A syntactic exception is a syntactic factor that is checked for
    self-reference in this implementation.
-}
syntacticException :: Parser SyntaxTree
syntacticException = do
    st <- syntacticFactor
    return (replaceIdentifier "syntactic exception" st)

syntacticFactor :: Parser SyntaxTree
syntacticFactor = do
    pos <- getPosition
    blPre <- irrelevent
    repeatBlock <- option [] (do
        repeatSym <- primST (string "*") "repetition symbol"
        blInListA <- irrelevent
        integer <- primST (many1 digit) "integer"
        return [repeatSym, blInListA, integer])
    blA <- irrelevent
    prim <- syntacticPrimary
    blPost <- irrelevent
    return (SyntaxTree "syntactic factor" "" pos ((blPre:repeatBlock) ++ [blA, prim, blPost]))

{-|

-}
syntacticPrimary :: Parser SyntaxTree
syntacticPrimary = do
    pos <- getPosition
    blPre <- irrelevent
    ch <- groupedSequence
      <|> optionalSequence
      <|> repeatedSequence
      <|> specialSequence
      <|> metaIdentifier
      <|> terminalString
      <|> emptySequence
    return (SyntaxTree "syntactic primary" "" pos [ch])

emptySequence :: Parser SyntaxTree
emptySequence = nullParser

optionalSequence :: Parser SyntaxTree
optionalSequence = do
    pos <- getPosition
    string "[" <|> string "(/"
    block <- definitionsList
    string "]" <|> string "/)"
    return (SyntaxTree "optional sequence" "" pos [block])

repeatedSequence :: Parser SyntaxTree
repeatedSequence = do
    pos <- getPosition
    string "(:" <|> string "{"
    block <- definitionsList
    string ":)" <|> string "}"
    return (SyntaxTree "repeated sequence" "" pos [block])

groupedSequence :: Parser SyntaxTree
groupedSequence = do
    pos <- getPosition
    string "("
    block <- definitionsList
    string ")"
    return (SyntaxTree "grouped sequence" "" pos [block])

terminalString :: Parser SyntaxTree
terminalString = do
    pos <- getPosition
    termstr <- quotedString '"' <|> quotedString '\''
    return $ SyntaxTree "terminal string" termstr pos []

specialSequence :: Parser SyntaxTree
specialSequence = do
    pos <- getPosition
    specialSeq <- quotedString '?'
    return (SyntaxTree "special sequence" specialSeq pos [])

quotedString :: Char -> Parser String
quotedString quoter = do
    char quoter
    cont <- many (syntacticExceptionCombinator anyCharSW (string [quoter]))
    char quoter
    return (concat cont)

escapedChar' :: Char -> Parser String
escapedChar' c = do
    esc <- many (string "\\\\")
    ch <- string ['\\', c]
    return $ concat esc ++ ch

metaIdentifier :: Parser SyntaxTree
metaIdentifier = do
    pos <- getPosition
    ident <- do
        h <- letter <|> char '_'
        t <- many $ letter <|> space <|> digit <|> char '_'
        return (h:t)
    return (SyntaxTree "meta identifier" (strip ident) pos [])


{-|
    Parser for irrelevent data, things like whitespace and comments. still
    parsed and added to the tree but grouped together
-}
irrelevent :: Parser SyntaxTree
irrelevent = do
    pos <- getPosition
    ch <- try $ many (comment <|> whitespaceST)
    return (SyntaxTree "irrelevent" "" pos ch)

nullParser :: Parser SyntaxTree
nullParser = do
    pos <- getPosition
    return (SyntaxTree "null" "" pos [])

comment :: Parser SyntaxTree
comment = do
    pos <- getPosition
    try $ string "(*"
    ch <- manyTill anyCharSW (try (string "*)"))
    return (SyntaxTree "comment" (concat ch) pos [])

commentSymbol :: Parser SyntaxTree
commentSymbol = do
    pos <- getPosition
    ch <- comment <|> terminalString <|> specialSequence <|> commentCharacterST
    return (SyntaxTree "comment symbol" "" pos [ch])

commentCharacterST :: Parser SyntaxTree
commentCharacterST = do
    pos <- getPosition
    ch <- manyTill anyChar $ eofStr <|> tryRS (string "*)")
    return (SyntaxTree "comment character" ch pos [])

whitespaceST :: Parser SyntaxTree
whitespaceST = do
    pos <- getPosition
    ch <- string " "
      <|> string "\n"
      <|> string "\f"
      <|> string "\v"
      <|> string "\t"
    return (SyntaxTree "whitespace" ch pos [])

anyCharSW :: Parser String
anyCharSW = do
    c <- escapedChar <|> anyChar
    return [c]

escapedChar :: Parser Char
escapedChar = char '\\' >> choice (zipWith escape codes replacements)
    where
        codes = "0abfnrtv\"&\'\\"
        replacements = "\0\a\b\f\n\r\t\v\"\&\'\\"

escape :: Char -> Char -> Parser Char
escape code replace = char code >> return replace

tryRS :: Parser a -> Parser String
tryRS par = do
    try par
    return ""

eofStr :: Parser String
eofStr = do
    try eof
    return ""

unescape :: String -> String
unescape []       = []
unescape [a]      = [a]
unescape (a:b:xs) = let esc  = ("0abfnrtv\"&'\\", "\0\a\b\f\n\r\t\v\"\&\'\\")
                        esc' = uncurry zip esc
                    in if (a == '\\') && (elem b . fst $ esc) then
                        (fromJust . lookup b $ esc'):unescape xs
                        else a:unescape (b:xs)
