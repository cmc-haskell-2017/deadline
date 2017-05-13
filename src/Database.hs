{-# LANGUAGE OverloadedStrings #-}

-- it's John Kim's code. Do never think to copy / paste :)

module Database where

import           Control.Applicative
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Control.Monad

-- "структура таблицы в базе данных для игры"
data GameDb = GameDb {playerId :: T.Text, playerName :: T.Text, score :: Int} deriving (Show)

-- "структура числового строки-результата запроса SQL"
data IntValueRow = IntValueRow (Maybe Int) deriving (Show)

-- "структура символьного строки-результата запроса SQL"
data TextValueRow = TextValueRow (Maybe T.Text) deriving (Show)

instance FromRow GameDb where
  fromRow = GameDb <$> field <*> field <*> field

instance FromRow IntValueRow where
  fromRow = IntValueRow <$> field

instance FromRow TextValueRow where
  fromRow = TextValueRow <$> field

instance ToRow GameDb where
  toRow (GameDb playerId playerName score) = toRow (playerId, playerName, score)

-- | Создание таблицы игры, если отсутствует таблица
createTable :: IO ()
createTable = do
  conn <- open "game.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS game (playerId TEXT, playerName TEXT, score INTEGER)"

-- | Создание данных для игрока
createPlayerRecord :: String -> String -> Int -> IO ()
createPlayerRecord playerId name score = do
  conn <- open "game.db"
  execute conn "INSERT INTO game (playerId, playerName, score) VALUES (?, ?, ?)" (GameDb (T.pack playerId) (T.pack name) score)
  close conn

-- | Обновление данных для игрока, причем обновляется только если игрок набрал больше баллов чем раньше
updatePlayerRecord :: String -> Int -> Int -> IO ()
updatePlayerRecord id currentScore lastScore = do
    conn <- open "game.db"
    when (currentScore > lastScore) $ executeNamed conn "UPDATE game SET score = :score WHERE playerId = :playerId" [":score" := currentScore, ":playerId" := (T.pack id)]
    close conn

-- | Если есть данные об игроке в базе, то берем максимум баллов
getPlayerScoreFromDb :: String -> IO Int
getPlayerScoreFromDb id = do
  conn <- open "game.db" 
  r <- queryNamed conn "SELECT MAX(score) from game WHERE playerId = :playerId" [":playerId" := (T.pack id)] :: IO [IntValueRow]
  let score = getScore r
  return score

-- | Инициализация игры с базы данных, если есть сохраненная игра, то берется старые данные, иначе новые
getGame :: String -> String -> IO Int
getGame id name = do
  conn <- open "game.db"
  r <- queryNamed conn "SELECT playerId from game WHERE playerId = :playerId" [":playerId" := (T.pack id)] :: IO [TextValueRow]
  maxScore <- (getPlayer r id name)
  return maxScore

-- | Берем Just из Maybe
getJustValue :: Maybe a -> a
getJustValue (Just value) = value

-- | Инициализация игрока с базы данных, если есть сохраненный игрок, то берется старые баллы, иначе 0
getPlayer :: [TextValueRow] -> String -> String -> IO Int
getPlayer [] id name = do
  createPlayerRecord id name 0
  return 0
getPlayer ((TextValueRow value):xs) id name = do 
  score <- (getPlayerScoreFromDb (T.unpack (getJustValue value)))
  return score

-- | От результата запроса возвращаем числовую величину (баллы в частности)
getScore :: [IntValueRow] -> Int  
getScore [] = 0
getScore ((IntValueRow value):xs) = (getJustValue value)

-- | От результата запроса возвращаем структуру таблицы игры
getPlayerRecord :: String -> IO [GameDb]
getPlayerRecord id = do
  conn <- open "game.db"
  r <- queryNamed conn "SELECT * from game WHERE playerId = :playerId" [":playerId" := (T.pack id)] :: IO [GameDb]
  close conn
  return r

-- | От результата запроса возвращаем первые 10 лучших игроков в базе данных
getRanking :: IO [GameDb]
getRanking = do
  conn <- open "game.db"
  r <- query_ conn "SELECT * from game ORDER BY score DESC LIMIT 10;" :: IO [GameDb]
  close conn
  return r