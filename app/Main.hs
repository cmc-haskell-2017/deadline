module Main where

import Deadline
import Database

main :: IO ()
main = do
  setPlayerRecord "john" 111
  record <- getPlayerRecord "john"
  print (getPlayerName (record !! 0)) --score must be used
  print (getScore (record !! 0)) --score must be used
  images <- loadImages
  runDeadline images