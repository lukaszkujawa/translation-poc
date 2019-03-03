module Lib
    ( someFunc
    ) where

import Data.String
import Data.List
import Text.Parsec
import Control.Monad

import Control.Monad.Reader


import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

-- http://book.realworldhaskell.org/read/using-parsec.html
-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

data LukeLang = String String | Number Int deriving (Show)

data Settings = Settings {
      param1 :: String
    , param2 :: String
}

--data TreeLang = TreeLangNode Node | TreeLangLeaf (Leaf Int) deriving (Show)

parseString :: Parser LukeLang
parseString = do
                char '|'
                x <- many (noneOf "|")
                char '|'
                return $ String x

parseNumber :: Parser LukeLang
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LukeLang
parseExpr =  parseString <|> parseNumber

run_parser :: Parser a -> String -> a
run_parser parser input =  case parse parser "" input of
    Left err -> error $ "parse error at " ++ (show err)
    Right val  -> val

readerDemo :: String
readerDemo = do
  let s = Settings {param1 = "Foo", param2 = "Bar"}
  runReader func1 s

func1 :: Reader Settings String
func1 = do
  val <- func2
  return ("func1() -> " ++ val)

func2 :: Reader Settings String
func2 = do
  val <- ask
  return $ param2 val

someFunc :: IO ()
someFunc = do
  print $ run_parser (many (space >> parseExpr)) " |my string| |hello| 5554"


  print readerDemo
