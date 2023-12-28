{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (forM)
import Control.Monad.State (State, get, put, runState)
import Data.Either (fromRight)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Text.Parsec qualified as P
import Text.RawString.QQ (r)

input =
  [r|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11|]

main = do
  ip <- lines <$> getContents
  -- let ip = lines input
  let inputs = map parseLines ip
  let state = do
        helperFunction inputs
  let (a, s) = runState state $ map ((,1) . (\x -> x.id)) inputs
  print $ round $ sum a
  print $ sum $ map snd s

helperFunction :: [Card] -> State [(Int, Int)] [Double]
helperFunction inputs = forM inputs \input -> do
  countList <- get
  let idx = fromJust $ findIndex (\x -> fst x == input.id) countList
  let (a, d) = splitAt (idx + 1) countList
  let adder = snd $ countList !! idx
  let result = filter (\x -> x `elem` input.owned) input.winning
  let (b, c) = splitAt (length result) d
  let b' = map (\x -> (fst x, adder + snd x)) b
  put $ a <> b' <> c
  let (xs1, xs2) = splitAt 1 result
  let a = fromIntegral (length xs1) + sum (map (2 **) $ take (length xs2) [0 ..])
  return a

data Card = Card {id :: Int, winning :: [Int], owned :: [Int]} deriving (Show)

parseLines :: String -> Card
parseLines string = fromRight default_ $ P.runParser parser () "" string
  where
    default_ = Card {id = 0, winning = [], owned = []}

parser = do
  _ <- P.string "Card"
  _ <- P.spaces
  card <- P.many P.digit
  P.string ":"
  _ <- P.spaces
  winning <- P.many parseNum
  _ <- P.string "|"
  _ <- P.spaces
  owned <- P.many parseNum
  return $ Card {id = read card, winning = winning, owned = owned}

parseNum = do
  num <- P.many1 P.digit
  P.optional P.spaces
  return $ read num
