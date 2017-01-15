module Main where

import Lib as L
import Text.Megaparsec as M

fileName :: String
fileName = "./input.md"

main :: IO ()
main = do
  input <- readFile fileName
  putStrLn . toString $ L.parseAll fileName input where
    toString :: Either (ParseError Char Dec) String -> String
    toString (Right x) = x
    toString (Left err) = M.parseErrorPretty err
