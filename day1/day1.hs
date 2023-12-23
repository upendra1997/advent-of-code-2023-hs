{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (digitToInt, isNumber)
import Data.Either (fromRight, rights)
import Debug.Trace (trace)
import Text.Parsec qualified as P
import Text.Printf (printf)

main :: IO ()
main = do
  ip <- lines <$> getContents
  let numbers = map parseNumbers ip
  print $ sum numbers

getNumber :: String -> Int
getNumber string =
  let numbers = map (read . printf "%c") $ filter isNumber string
   in head numbers * 10 + last numbers

parseNumbers :: String -> Int
parseNumbers string =
  let resultL = P.runParser (parser id) () "" string
      resultR = P.runParser (parser reverse) () "" (reverse string)
      numbersL = filter (>= 0) $ fromRight [0] resultL
      numbersR = filter (>= 0) $ fromRight [0] resultR
      caliberation = head numbersL * 10 + head numbersR
   in caliberation
  where
    parser f = P.many $ P.choice (fmap (\x -> x f) [one, two, three, four, five, six, seven, eight, nine, number, err])
    one f = do
      a <- P.try $ P.string $ f "one"
      return 1
    two f = do
      a <- P.try $ P.string $ f "two"
      return 2
    three f = do
      a <- P.try $ P.string $ f "three"
      return 3
    four f = do
      a <- P.try $ P.string $ f "four"
      return 4
    five f = do
      a <- P.try $ P.string $ f "five"
      return 5
    six f = do
      a <- P.try $ P.string $ f "six"
      return 6
    seven f = do
      a <- P.try $ P.string $ f "seven"
      return 7
    eight f = do
      a <- P.try $ P.string $ f "eight"
      return 8
    nine f = do
      a <- P.try $ P.string $ f "nine"
      return 9
    err f = do
      _ <- P.anyChar
      return $ -1
    number f = do
      a <- P.try P.digit
      return $ digitToInt a
