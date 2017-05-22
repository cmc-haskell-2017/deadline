{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Applicative
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Control.Monad

-- "структура таблицы в базе данных для игры"
data GameDb = GameDb {gameId :: T.Text, score :: Int} deriving (Show)

-- "структура таблицы в базе данных для игры"
data PlayerDb = PlayerDb {playerId :: T.Text, playerPassword :: T.Text, playerName :: T.Text} deriving (Show)

-- "структура числового строки-результата запроса SQL"
data IntValueRow = IntValueRow (Maybe Int) deriving (Show)

-- "структура символьного строки-результата запроса SQL"
data TextValueRow = TextValueRow (Maybe T.Text) deriving (Show)

-- "структура символьного строки-результата запроса SQL"
data RankingRow = RankingRow {rankingPlayerName :: T.Text, rankingPlayerScore :: Int} deriving (Show)

instance FromRow GameDb where
  fromRow = GameDb <$> field <*> field

instance FromRow PlayerDb where
  fromRow = PlayerDb <$> field <*> field <*> field

instance FromRow IntValueRow where
  fromRow = IntValueRow <$> field

instance FromRow TextValueRow where
  fromRow = TextValueRow <$> field

instance FromRow RankingRow where
  fromRow = RankingRow <$> field <*> field

instance ToRow GameDb where
  toRow (GameDb gameId score) = toRow (gameId, score)

instance ToRow PlayerDb where
  toRow (PlayerDb playerId playerPassword playerName) = toRow (playerId, playerPassword, playerName)

-- | Создание таблицы игры, если отсутствует таблица
createTable :: IO ()
createTable = do
  conn <- open "game.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS game (gameId TEXT, score INTEGER)"
  execute_ conn "CREATE TABLE IF NOT EXISTS player (playerId TEXT, playerPassword TEXT, playerName TEXT)"

-- | Создание данных для игры
createGameRecord :: String -> Int -> IO ()
createGameRecord gameId score = do
  conn <- open "game.db"
  execute conn "INSERT INTO game (gameId, score) VALUES (?, ?)" (GameDb (T.pack gameId) score)
  close conn

-- | Создание данных для игрока
createPlayerRecord :: String -> String -> String -> IO ()
createPlayerRecord playerId playerPassword name = do
  conn <- open "game.db"
  execute conn "INSERT INTO player (playerId, playerPassword, playerName) VALUES (?, ?, ?)" (PlayerDb (T.pack playerId) (T.pack playerPassword) (T.pack name))
  close conn

-- | Обновление данных игры, причем обновляется только если игрок набрал больше баллов чем раньше
updatePlayerRecord :: String -> Int -> Int -> IO ()
updatePlayerRecord id currentScore lastScore = do
    conn <- open "game.db"
    when (currentScore > lastScore) $ executeNamed conn "UPDATE game SET score = :score WHERE gameId = :gameId" [":score" := currentScore, ":gameId" := (T.pack id)]
    close conn

-- | Если есть данные об игроке в базе, то берем максимум его баллов
getPlayerScoreFromDb :: String -> IO Int
getPlayerScoreFromDb id = do
  conn <- open "game.db" 
  r <- queryNamed conn "SELECT MAX(score) from game WHERE gameId = :gameId" [":gameId" := (T.pack id)] :: IO [IntValueRow]
  let score = getScore r
  return score

-- | Осуществляем вход, если пользователь с таким логином не существует, то просим его имя и регистрируем его
loginPlayer :: String -> String -> IO Int
loginPlayer id password = do
  conn <- open "game.db"
  r <- queryNamed conn "SELECT playerId from player WHERE playerId = :playerId AND playerPassword = :playerPassword" [":playerId" := (T.pack id), ":playerPassword" := (T.pack password)] :: IO [TextValueRow]
  let isRegistered = checkPlayerRegistered r
  score <- (registerPlayer isRegistered id password)
  return score

-- | Регистрация пользователя, запрашиваем имя
registerPlayer :: Bool -> String -> String -> IO Int
registerPlayer isRegistered id password 
  | isRegistered = do 
    score <- (getGame id)
    return score
  | otherwise = do
    putStrLn "for registering, please enter your name"
    name <- getLine
    createPlayerRecord id password name
    score <- (getGame id)
    return score

-- | Инициализация игры с базы данных, если есть сохраненная игра, то берется старые данные, иначе новые
getGame :: String -> IO Int
getGame id = do
  conn <- open "game.db"
  r <- queryNamed conn "SELECT gameId from game WHERE gameId = :gameId" [":gameId" := (T.pack id)] :: IO [TextValueRow]
  maxScore <- (getGameScore r id)
  return maxScore

-- | Берем Just из Maybe
getJustValue :: Maybe a -> a
getJustValue (Just value) = value

-- | Проверка результата возврата с бд, если что-то возвращается (непустой список), то это значит, что уже есть такой пользователь
checkPlayerRegistered :: [TextValueRow] -> Bool
checkPlayerRegistered [] = False
checkPlayerRegistered ((TextValueRow value):xs) = True

-- | Инициализация баллов игрока с базы данных, если есть сохраненные баллы игрока, то берется старые баллы, иначе 0
getGameScore :: [TextValueRow] -> String -> IO Int
getGameScore [] id = do
  createGameRecord id 0
  return 0
getGameScore ((TextValueRow value):xs) _ = do 
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
  r <- queryNamed conn "SELECT * from game WHERE gameId = :gameId" [":gameId" := (T.pack id)] :: IO [GameDb]
  close conn
  return r

-- | От результата запроса возвращаем первые 10 лучших игроков в базе данных
getRanking :: IO [RankingRow]
getRanking = do
  conn <- open "game.db"
  r <- query_ conn "SELECT player.playerName, game.score FROM game INNER JOIN player ON game.gameId=player.playerId ORDER BY score DESC LIMIT 10 ;" :: IO [RankingRow]
  close conn
  return r