module Main where

import Deadline
import Database
import Types

main :: IO ()
main = do
  createTable
  putStrLn "enter your username: "
  id <- getLine
  putStrLn "enter your password: "
  password <- getLine
  score <- loginPlayer id password
  images <- loadImages
  runDeadline images id score