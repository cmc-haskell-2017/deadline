module Main where

import Deadline
import Database
import Types

main :: IO ()
main = do
  createTable
  putStrLn "enter your ID: "
  id <- getLine
  putStrLn "enter your Password: "
  password <- getLine
  score <- loginPlayer id password
  images <- loadImages
  runDeadline images id score