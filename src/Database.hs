{-# LANGUAGE OverloadedStrings #-}

-- it's John Kim's code. Do never think to copy / paste :)

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

setPlayerRecord :: String -> Int -> IO ()
setPlayerRecord name score = do
  conn <- open "players.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS players (playerId INTEGER PRIMARY KEY, playerName TEXT, score INTEGER)"
  rowId <- lastInsertRowId conn
  execute conn "INSERT INTO players (playerId, playerName, score) VALUES (?, ?, ?)" (Player (fromIntegral rowId) (T.pack name) score)
  close conn

updatePlayerRecord :: String -> Int -> IO ()
updatePlayerRecord name score = do
  conn <- open "players.db"
  executeNamed conn "UPDATE players SET score = :score WHERE playerName = :playerName" [":score" := score, ":playerName" := (T.pack name)]
  close conn

getScore :: Player -> Int  
getScore (Player _ _ score) = score  

getPlayerName :: Player -> String  
getPlayerName (Player _ name _) = T.unpack name  

getPlayerRecord :: String -> IO [Player]
getPlayerRecord playerName = do
  conn <- open "players.db"
  r <- queryNamed conn "SELECT * from players WHERE playerName = :playerName" [":playerName" := (T.pack playerName)] :: IO [Player]
  close conn
  return r

getRanking :: IO [Player]
getRanking = do
  conn <- open "players.db"
  r <- query_ conn "SELECT * from players ORDER BY score" :: IO [Player]
  close conn
  return r

removePlayerRecord :: String -> IO ()
removePlayerRecord name = do
  conn <- open "players.db"
  execute conn "DELETE from players WHERE playerName = ?" (Only name)
  close conn
  
 -- rowId <- lastInsertRowId conn
 -- executeNamed conn "UPDATE test SET str = :str WHERE id = :id" [":str" := ("updated str" :: T.Text), ":id" := rowId]
-- Record {score = (getScore (r !! 0)), name = playerName}