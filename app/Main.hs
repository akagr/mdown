module Main where

import Lib as L
import System.Environment as Env
import Text.Megaparsec as M

getFileName :: [String] -> String
getFileName [] = ""
getFileName (x:_) = x

main :: IO ()
main = do
  fileName <- fmap getFileName Env.getArgs
  input <- readFile fileName
  putStrLn . toString $ L.parseAll fileName input where
    toString :: Either (ParseError Char Dec) String -> String
    toString (Right x) = x
    toString (Left err) = M.parseErrorPretty err
