{-# LANGUAGE OverloadedStrings #-}

-- it's John Kim's code. Do never think to copy / paste :)

module Database where

import           Control.Applicative
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data Player = Player {playerId :: Int, playerName :: T.Text, score :: Int} deriving (Show)

data Id = Id (Maybe Int) deriving (Show)

instance FromRow Player where
  fromRow = Player <$> field <*> field <*> field

instance FromRow Id where
  fromRow = Id <$> field

instance ToRow Player where
  toRow (Player playerId playerName score) = toRow (playerId, playerName, score)

setPlayerRecord :: String -> Int -> IO Int
setPlayerRecord name score = do
  conn <- open "players.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS players (playerId INTEGER PRIMARY KEY, playerName TEXT, score INTEGER)"
  rowId <- generateNewId
  execute conn "INSERT INTO players (playerId, playerName, score) VALUES (?, ?, ?)" (Player rowId (T.pack name) score)
  close conn
  return rowId

updatePlayerRecord :: Int -> Int -> IO ()
updatePlayerRecord id score = do
  conn <- open "players.db"
  executeNamed conn "UPDATE players SET score = :score WHERE playerId = :playerId" [":score" := score, ":playerId" := id]
  close conn

generateNewId :: IO Int
generateNewId = do 
  conn <- open "players.db"
  r <- query_ conn "SELECT MAX(playerId) from players" :: IO [Id]
  let rowId = getUniqueIdentifier r
  return rowId

checkNullValue :: (Maybe Int) -> Int
checkNullValue (Just a) = a
checkNullValue Nothing = 0

getUniqueIdentifier :: [Id] -> Int
getUniqueIdentifier [] = 0
getUniqueIdentifier ((Id value):xs) = (checkNullValue (value)) + 1

getScore :: Player -> Int  
getScore (Player _ _ score) = score  

getPlayerName :: Player -> String  
getPlayerName (Player _ name _) = T.unpack name  

getPlayerRecord :: Int -> IO [Player]
getPlayerRecord id = do
  conn <- open "players.db"
  r <- queryNamed conn "SELECT * from players WHERE playerId = :playerId" [":playerId" := id] :: IO [Player]
  close conn
  return r
--
getRanking :: IO [Player]
getRanking = do
  conn <- open "players.db"
  r <- query_ conn "SELECT * from players ORDER BY score DESC LIMIT 10;" :: IO [Player]
  close conn
  return r

removePlayerRecord :: String -> IO ()
removePlayerRecord name = do
  conn <- open "players.db"
  execute conn "DELETE from players WHERE playerName = ?" (Only name)
  close conn