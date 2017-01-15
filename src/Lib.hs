module Lib (
    parseAll
  ) where


import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

parseAll :: String -> String -> Either (ParseError Char Dec) String
parseAll = parse (between sc eof allCombined)

allCombined :: Parsec Dec String String
allCombined = do
  x <- many $
    headingp <|>
    everything
  return $ concat x

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = void spaceChar
        blockCmnt = L.skipBlockComment "<!--" "-->"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Matches headings in #... format. Matches all six levels
headingLevel :: Parsec Dec String String
headingLevel = try $ do
  x <- count' 1 6 (char '#')
  skipMany spaceChar
  y <- some alphaNumChar
  let len = show $ length x
  return $ concat ["<h", len, ">", y, "</h", len, ">", "\n" ]

-- Matches headings in underlined sytax. Accepts line
-- character and heading level it corresponds to.
-- For example, '===========' corresponds to h1.
-- So the inputs will be '=' and 1.
headingLine :: Char -> Int -> Parsec Dec String String
headingLine c lvl = try $ do
  x <- many printChar
  _ <- some eol
  _ <- some (char c)
  _ <- some eol
  return $ concat ["<h", show lvl, ">", x, "</h", show lvl, ">", "\n" ]

headingp :: Parsec Dec String String
headingp = lexeme $ headingLevel <|>
  headingLine '=' 1 <|>
  headingLine '-' 2

everything :: Parsec Dec String String
everything = lexeme $ do
  x <- some printChar
  return $ x ++ "\n"
