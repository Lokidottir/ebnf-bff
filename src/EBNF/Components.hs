module EBNF.Components where

import EBNF.Helper
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Data.List

{-
    The majority of this file is copying the EBNF grammar as defined in EBNF
    to a set or rules that make up an EBNF parser with Parsec
-}

-- string wrapped letter and digit parsers
letterSW :: Parser String
letterSW = do
    a <- letter
    return [a]

digitSW :: Parser String
digitSW = do
    a <- digit
    return [a]

spaceSW :: Parser String
spaceSW = do
    a <- space
    return [a]

tabSW :: Parser String
tabSW = do
    a <- tab
    return [a]

integer :: Parser String
integer = many1 digit

-- rule: "concatenate symbol"
concatSymbol :: Parser String
concatSymbol = string ","

-- rule: "defining symbol"
defineSymbol :: Parser String
defineSymbol = string "="

-- rule: "definition separator symbol"
alternationSymbol :: Parser String
alternationSymbol = string "|" <|> string "/" <|> string "!"

-- rule: "start comment symbol"
commentSymbolBegin :: Parser String
commentSymbolBegin = string "(*"

-- rule: "end comment symbol"
commentSymbolEnd :: Parser String
commentSymbolEnd = string "*)"

-- rule: "start group symbol"
groupSymbolBegin :: Parser String
groupSymbolBegin = string "("

-- rule: "end group symbol"
groupSymbolEnd :: Parser String
groupSymbolEnd = string ")"

-- rule: "start option symbol"
optionSymbolBegin :: Parser String
optionSymbolBegin = string "[" <|> string "(/"

-- rule: "end option symbol"
optionSymbolEnd :: Parser String
optionSymbolEnd = string "]" <|> string "/)"

-- rule: "start repeat symbol"
repeatSymbolBegin :: Parser String
repeatSymbolBegin = string "{" <|> string "(:"

-- rule: "end repeat symbol"
repeatSymbolEnd :: Parser String
repeatSymbolEnd = string "}" <|> string ":)"

-- rule: "except symbol"
exceptSymbol :: Parser String
exceptSymbol = string "-"

-- rule: "first quote symbol"
quoteSingle :: Parser String
quoteSingle = string "'"

-- rule: "second quote symbol"
quoteDouble :: Parser String
quoteDouble = string "\""

-- rule: "repetition symbol"
repetitionSymbol :: Parser String
repetitionSymbol = string "*"

-- rule: "special sequence symbol"
specialSymbol :: Parser String
specialSymbol = string "?"

-- rule: "terminator symbol"
terminatorSymbol :: Parser String
terminatorSymbol = string ";" <|> string "."

-- rule: "other character"
miscChar :: Parser String
miscChar = do
    a <- oneOf " :+_%@&#$<>\\^`~"
    return [a]

-- rule: "terminal character"
terminalCharacter :: Parser String
terminalCharacter = letterSW <|>
                    digitSW  <|>
                    concatSymbol <|>
                    defineSymbol <|>
                    alternationSymbol <|>
                    commentSymbolBegin <|>
                    commentSymbolEnd <|>
                    groupSymbolBegin <|>
                    groupSymbolBegin <|>
                    optionSymbolBegin <|>
                    optionSymbolEnd <|>
                    repeatSymbolBegin <|>
                    repeatSymbolEnd <|>
                    exceptSymbol <|>
                    quoteSingle <|>
                    repetitionSymbol <|>
                    quoteDouble <|>
                    specialSymbol <|>
                    terminatorSymbol <|>
                    miscChar

commmentlessSymbol :: Parser String
commmentlessSymbol =
    (syntacticException
        terminalCharacter (letterSW
                       <|> digitSW
                       <|> quoteSingle
                       <|> quoteDouble
                       <|> commentSymbolBegin
                       <|> commentSymbolEnd
                       <|> specialSymbol
                       <|> miscChar))
    <|> metaIdentifier
    <|> integer
    <|> terminalString
    <|> specialSequence

-- rule: "gap free symbol"
gaplessSymbol :: Parser String
gaplessSymbol = do
    (syntacticException terminalCharacter (quoteSingle <|> quoteDouble)) <|>
     terminalString

-- rule: "terminal string"
terminalString :: Parser String
terminalString = do
    (quoteDoubleString) <|> (quoteSingleString)

gapSeperator :: Parser String
gapSeperator = spaceSW <|> tabSW <|> (string "\n") <|>
               (string "\v") <|> (string "\f")

escaped :: Parser String
escaped = do
    escape <- char '\\'
    character <- oneOf "\\\"0nrvtbf"
    return [escape, character]

quoteDoubleString :: Parser String
quoteDoubleString = do
    quoteDouble
    stringContent <- many (syntacticException terminalCharacter quoteDouble)
    quoteDouble
    return (concat stringContent)

quoteSingleString :: Parser String
quoteSingleString = do
    quoteSingle
    stringContent <- many (syntacticException terminalCharacter quoteSingle)
    quoteSingle
    return (concat stringContent)

-- rule: "meta identifier character"
metaCharacter :: Parser String
metaCharacter = letterSW <|> digitSW

-- rule: "meta identifier"
metaIdentifier :: Parser String
metaIdentifier = do
    a <- letterSW
    b <- many metaCharacter
    return (concat (a:b))

specialSequence :: Parser String
specialSequence = do
    specialSymbol
    a <- many (syntacticException terminalCharacter specialSymbol)
    specialSymbol
    return (concat a)
