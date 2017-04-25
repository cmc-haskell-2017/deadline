module Main where

import Deadline
import Database

main :: IO ()
main = do
  testDb 111
  images <- loadImages
  runDeadline images