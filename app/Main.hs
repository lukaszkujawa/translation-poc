{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import System.Environment
import Tokenizer

main :: IO ()
main = do
  args <- getArgs
  events <- from_file "example.json"
  putStrLn "Translation POC"
  print events
