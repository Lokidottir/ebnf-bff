module EBNF.Components where

import EBNF.Helper
import EBNF.SyntaxTree
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Data.List

{-
    The majority of this file is copying the EBNF grammar as defined in EBNF
    to a set or rules that make up an EBNF parser with Parsec
-}

{-

-}
primST :: Parser String -> String -> Parser SyntaxTree
primST par name = do
    pos <- getPosition
    text <- par
    return (SyntaxTree name text pos [])

letterST :: Parser SyntaxTree
letterST = primST letterSW "_letter_"

-- string wrapped letter and digit parsers
letterSW :: Parser String
letterSW = do
    a <- letter
    return [a]

digitST :: Parser SyntaxTree
digitST = primST digitSW "_digit_"

digitSW :: Parser String
digitSW = do
    a <- digit
    return [a]

spaceST :: Parser SyntaxTree
spaceST = primST spaceSW "_space_"

spaceSW :: Parser String
spaceSW = do
    a <- space
    return [a]

tabST :: Parser SyntaxTree
tabST = primST (string "\t") "_tab_"

tabSW :: Parser String
tabSW = do
    a <- tab
    return [a]

integer :: Parser SyntaxTree
integer = primST (many1 digit) "integer"

-- rule: "concatenate symbol"
concatSymbol :: Parser SyntaxTree
concatSymbol = primST (string ",") "concatenate symbol"

-- rule: "defining symbol"
defineSymbol :: Parser SyntaxTree
defineSymbol = primST (string "=") "defining symbol"

-- rule: "definition separator symbol"
alternationSymbol :: Parser SyntaxTree
alternationSymbol = primST (string "|" <|> string "/" <|> string "!") "definition separator symbol"

-- rule: "start comment symbol"
commentSymbolBegin :: Parser SyntaxTree
commentSymbolBegin = primST (string "(*") "start comment symbol"

-- rule: "end comment symbol"
commentSymbolEnd :: Parser SyntaxTree
commentSymbolEnd = primST (string "*)") "end comment symbol"

-- rule: "start group symbol"
groupSymbolBegin :: Parser SyntaxTree
groupSymbolBegin = primST (string "(") "start group symbol"

-- rule: "end group symbol"
groupSymbolEnd :: Parser SyntaxTree
groupSymbolEnd = primST (string ")") "end group symbol"

-- rule: "start option symbol"
optionSymbolBegin :: Parser SyntaxTree
optionSymbolBegin = primST (string "[" <|> string "(/") "start option symbol"

-- rule: "end option symbol"
optionSymbolEnd :: Parser SyntaxTree
optionSymbolEnd = primST (string "]" <|> string "/)")  "end option symbol"

-- rule: "start repeat symbol"
repeatSymbolBegin :: Parser SyntaxTree
repeatSymbolBegin = primST (string "{" <|> string "(:") "start repeat symbol"

-- rule: "end repeat symbol"
repeatSymbolEnd :: Parser SyntaxTree
repeatSymbolEnd = primST (string "}" <|> string ":)") "end repeat symbol"

-- rule: "except symbol"
exceptSymbol :: Parser SyntaxTree
exceptSymbol = primST (string "-") "except symbol"

-- rule: "first quote symbol"
quoteSingle :: Parser SyntaxTree
quoteSingle = primST (string "'") "first quote symbol"

-- rule: "second quote symbol"
quoteDouble :: Parser SyntaxTree
quoteDouble = primST (string "\"") "second quote symbol"

-- rule: "repetition symbol"
repetitionSymbol :: Parser SyntaxTree
repetitionSymbol = primST (string "*") "repetition symbol"

-- rule: "special sequence symbol"
specialSymbol :: Parser SyntaxTree
specialSymbol = primST (string "?") "special sequence symbol"

-- rule: "terminator symbol"
terminatorSymbol :: Parser SyntaxTree
terminatorSymbol = primST (string ";" <|> string ".") "terminator symbol"

-- rule: "other character"
miscCharSW :: Parser String
miscCharSW = do
    a <- oneOf " :+_%@&#$<>\\^`~"
    return [a]

miscChar :: Parser SyntaxTree
miscChar = primST miscCharSW "_other character_"

-- rule: "terminal character"
terminalCharacter :: Parser SyntaxTree
terminalCharacter = do
    pos <- getPosition
    child <- (letterST
          <|> digitST
          <|> concatSymbol
          <|> defineSymbol
          <|> alternationSymbol
          <|> commentSymbolBegin
          <|> commentSymbolEnd
          <|> groupSymbolBegin
          <|> groupSymbolBegin
          <|> optionSymbolBegin
          <|> optionSymbolEnd
          <|> repeatSymbolBegin
          <|> repeatSymbolEnd
          <|> exceptSymbol
          <|> quoteSingle
          <|> repetitionSymbol
          <|> quoteDouble
          <|> specialSymbol
          <|> terminatorSymbol
          <|> miscChar)
    return (SyntaxTree "terminal character" "" pos [child])


commmentlessSymbol :: Parser SyntaxTree
commmentlessSymbol = do
    pos <- getPosition
    child <- ((syntacticException
               terminalCharacter (letterST
                              <|> digitST
                              <|> quoteSingle
                              <|> quoteDouble
                              <|> commentSymbolBegin
                              <|> commentSymbolEnd
                              <|> specialSymbol
                              <|> miscChar))
             <|> metaIdentifier
             <|> integer
             <|> terminalString
             <|> specialSequence)
    return (SyntaxTree "commentless symbol" "" pos [child])

-- rule: "gap free symbol"
gaplessSymbol :: Parser SyntaxTree
gaplessSymbol = do
    pos <- getPosition
    child <- ((syntacticException terminalCharacter (quoteSingle <|> quoteDouble))
             <|> terminalString)
    return (SyntaxTree "gap free symbol" "" pos [child])


-- rule: "terminal string"
terminalString :: Parser SyntaxTree
terminalString = do
    pos <- getPosition
    child <- ((quoteDoubleString) <|> (quoteSingleString))
    return (SyntaxTree "terminal string" "" pos [child])

gapSeperator :: Parser SyntaxTree
gapSeperator = primST (spaceSW <|> tabSW <|> (string "\n") <|>
                        (string "\v") <|> (string "\f")) "gap separator"

escaped :: Parser String
escaped = do
    escape <- char '\\'
    character <- oneOf "\\\"0nrvtbf"
    return [escape, character]

quoteDoubleString :: Parser SyntaxTree
quoteDoubleString = do
    pos <- getPosition
    quoteDouble
    stringContent <- many (syntacticException terminalCharacter quoteDouble)
    quoteDouble
    return (SyntaxTree "_terminal string_" "" pos stringContent)

quoteSingleString :: Parser SyntaxTree
quoteSingleString = do
    pos <- getPosition
    quoteSingle
    stringContent <- many (syntacticException terminalCharacter quoteSingle)
    quoteSingle
    return (SyntaxTree "_terminal string_" "" pos stringContent)

-- rule: "meta identifier character"
metaCharacter :: Parser SyntaxTree
metaCharacter = primST (letterSW <|> digitSW) "_meta identifier character_"

-- rule: "meta identifier"
metaIdentifier :: Parser SyntaxTree
metaIdentifier = do
    pos <- getPosition
    a <- letterST
    b <- many metaCharacter
    return (SyntaxTree "meta identifier" "" pos (a:b))

-- rule "special sequence"
specialSequence :: Parser SyntaxTree
specialSequence = do
    pos <- getPosition
    specialSymbol
    a <- many (syntacticException terminalCharacter specialSymbol)
    specialSymbol
    return (SyntaxTree "special sequence" "" pos a)

commentSymbol :: Parser SyntaxTree
commentSymbol = do
    pos <- getPosition
    child <- (bracketedComment <|> miscChar <|> commmentlessSymbol)
    return (SyntaxTree "comment symbol" "" pos [child])

-- rule: "bracketed textual comment"
bracketedComment :: Parser SyntaxTree
bracketedComment = do
    pos <- getPosition
    children <- do
        begin <- commentSymbolBegin
        commentText <- many commentSymbol
        end <- commentSymbolEnd
        return ([begin] ++ commentText ++ [end])
    return (SyntaxTree "bracketed textual comment" "" pos children)

commentSyntax :: Parser SyntaxTree
commentSyntax = do
    pos <- getPosition
    children <- do
        commentsBlockA <- many bracketedComment
        nonCBlock <- commmentlessSymbol
        commentsBlockB <- many bracketedComment
        repeatedBlocks <- many (do
            nonCBlockRep <- commmentlessSymbol
            commentsBlockRep <- many bracketedComment
            return (nonCBlockRep:commentsBlockRep))
        return (commentsBlockA ++ [nonCBlock] ++ commentsBlockB ++ (concat repeatedBlocks))
    return (SyntaxTree "syntax (comments)" "" pos children)
