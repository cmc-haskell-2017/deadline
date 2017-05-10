module Main where

import Deadline
import Database
import Types

main :: IO ()
main = do
  name <- getLine
  setPlayerRecord name 0
  images <- loadImages
  runDeadline images name