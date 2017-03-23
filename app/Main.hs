module Main where

import Deadline

main :: IO ()
main = do
	images <- loadImages
 	runDeadline images