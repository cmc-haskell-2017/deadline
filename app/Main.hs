module Main where

import Deadline
import Database
import Types

main :: IO ()
main = do
  createTable
  putStrLn "enter your ID"
  id <- getLine
  putStrLn "enter your name"
  name <- getLine
  score <- getGame id name
  images <- loadImages
  runDeadline images name id score