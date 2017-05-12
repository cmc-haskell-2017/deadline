module Main where

import Deadline
import Database
import Types

main :: IO ()
main = do
  name <- getLine
  id <- (setPlayerRecord name 0)
  images <- loadImages
  runDeadline images name id