{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Either (fromRight)
import Text.Parsec qualified as P

data Turn = Turn {red :: Int, green :: Int, blue :: Int} deriving (Show)

data Game = Game {id :: Int, turns :: [Turn]} deriving (Show)

main = do
  ip <- lines <$> getContents
  let games = map parseGames ip
  print $ sum $ map (\x -> x.id) $ filter validGame games
  print $ sum $ map power games

parseGames :: String -> Game
parseGames string =
  fromRight (Game {id = minBound, turns = []}) $
    P.runParser
      do
        _ <- P.string "Game"
        _ <- P.spaces
        id <- P.many P.digit
        _ <- P.string ":"
        turns <- parseTurns
        return $ Game {id = read id, turns = turns}
      ()
      ""
      string

parseTurns = P.many $ do
  values <- P.many1 $ do
    _ <- P.spaces
    count <- P.many1 P.digit
    _ <- P.spaces
    color <- P.choice [P.string "red", P.string "blue", P.string "green"]
    _ <- P.optional $ P.string ","
    return (read count, color)
  let red = sum $ map fst $ filter (\x -> snd x == "red") values
  let blue = sum $ map fst $ filter (\x -> snd x == "blue") values
  let green = sum $ map fst $ filter (\x -> snd x == "green") values
  _ <- P.optional $ P.string ";"
  return $ Turn {red = red, blue = blue, green = green}

validGame :: Game -> Bool
validGame game = length game.turns == length (filter (\x -> x.red <= 12 && x.green <= 13 && x.blue <= 14) game.turns)

power :: Game -> Int
power game =
  let maxRed = maximum $ map (\x -> x.red) game.turns
      maxBlue = maximum $ map (\x -> x.blue) game.turns
      maxGreen = maximum $ map (\x -> x.green) game.turns
   in maxRed * maxBlue * maxGreen