module Lib (
    parseAll
  ) where


import Control.Monad (void)
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

parseAll :: String -> String -> Either (ParseError Char Dec) String
parseAll = parse (between sc eof allCombined)

allCombined :: Parser String
allCombined = do
  x <- lexeme $ many (
      try headingp <|>
      try link <|>
      try bold <|>
      try italics <|>
      --try lists <|>
      try codeBlock <|>
      try inlineCode <|>
      try lineBreak <|>
      try everything
    )
  return $ concat x

sc :: Parser ()
sc = L.space (void spaces) lineCmnt blockCmnt
  where lineCmnt  = void spaces
        blockCmnt = L.skipBlockComment "<!--" "-->"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

spaces :: Parser Char
spaces = char ' ' <|> char '\t'

-- Replace every two newline characters with an html line-break
lineBreak :: Parser String
lineBreak = do
  x <- some eol
  let numBreaks = quot (length x) 2
  return $ concat . take numBreaks $ repeat "<br/>"

link :: Parser String
link = do
  _ <- char '['
  bang <- count' 0 1 (char '!')
  txt <- someTill anyChar (char ']')
  skipMany spaces
  _ <- char '('
  lnk <- someTill anyChar (char ')')
  return $ concat ["<a href=\"", lnk, "\"", isNewPage bang, ">", txt, "</a>"] where
    isNewPage bang = if not $ null bang then "target=\"_blank\"" else ""

-- Generic parser to replace provided delimiters with given
-- html tags
surrounds :: Parser a -> String -> String -> Parser String
surrounds p start end = do
  _ <- p
  x <- someTill anyChar p
  return $ concat [start, x, end]

italics :: Parser String
italics = surrounds (count 1 (char '*')) "<em>" "</em>"

bold :: Parser String
bold = surrounds (count 2 (char '*')) "<strong>" "</strong>"

inlineCode :: Parser String
inlineCode = surrounds (char '`') "<code>" "</code>"

codeBlockDelimiter :: Parser String
codeBlockDelimiter = do
  _ <- string "```"
  skipMany spaces
  many printChar

codeBlock :: Parser String
codeBlock = surrounds codeBlockDelimiter "<pre><code>" "</code></pre>"

listItem :: Parser String
listItem = do
  skipMany spaces
  _ <- char '*'
  skipMany spaces
  item <- someTill printChar eol
  skipMany eol
  return item

lists :: Parser String
lists = do
  items <- some listItem
  return ("<ul>" ++ concat (wrapLi items) ++ "</ul>") where
    wrapLi = fmap (\i -> "<li>" ++ i ++ "</li>")

-- Matches headings in #... format. Matches all six levels
headingLevel :: Parser String
headingLevel = do
  x <- count' 1 6 (char '#')
  skipMany spaces
  y <- some printChar
  skipMany eol
  let len = show $ length x
  return $ concat ["<h", len, ">", y, "</h", len, ">", "\n" ]

-- Matches headings in underlined sytax. Accepts line
-- character and heading level it corresponds to.
-- For example, '===========' corresponds to h1.
-- So the inputs will be '=' and 1.
headingLine :: Char -> Int -> Parser String
headingLine c lvl = do
  x <- some printChar
  skipMany eol
  _ <- some (char c)
  skipMany eol
  return $ concat ["<h", show lvl, ">", x, "</h", show lvl, ">", "\n" ]

headingp :: Parser String
headingp = headingLevel <|>
  headingLine '=' 1 <|>
  headingLine '-' 2

everything :: Parser String
everything = do
  x <- anyChar
  return [x]
