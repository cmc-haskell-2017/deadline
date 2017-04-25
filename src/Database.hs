{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Applicative
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data Player = Player Int T.Text Int deriving (Show)

instance FromRow Player where
  fromRow = Player <$> field <*> field <*> field

instance ToRow Player where
  toRow (Player playerId playerName score) = toRow (playerId, playerName, score)

setPlayerRecord :: Int -> IO ()
setPlayerRecord score = do
  conn <- open "players.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS players (playerId INTEGER PRIMARY KEY, playerName TEXT, score INTEGER)"
  execute conn "INSERT INTO players (playerId, playerName, score) VALUES (?, ?, ?)" (Player 1 "john" score)
  r <- query_ conn "SELECT * from players" :: IO [Player]
  print r
  execute_ conn "DELETE FROM players WHERE playerId = 1"
  close conn
 -- rowId <- lastInsertRowId conn
 -- executeNamed conn "UPDATE test SET str = :str WHERE id = :id" [":str" := ("updated str" :: T.Text), ":id" := rowId]]
