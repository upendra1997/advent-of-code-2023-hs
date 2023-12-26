{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (guard, mapM_)
import Control.Monad.State (State, evalState, get, put)
import Data.Either (fromRight)
import Data.List (foldl, foldr, groupBy, nub, sort, tails)
import Data.Tree (Tree, drawForest, flatten, levels, unfoldForestM_BF)
import Debug.Trace (trace)
import Text.Parsec qualified as P
import Text.Printf (printf)
import Text.RawString.QQ (r)

data Element = None | Symbol Char | Element Int deriving (Show, Eq, Ord)

input =
  [r|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..|]

type Coord = (Int, Int)

type MatrixElement = (Coord, Element)

takeLengthOf = zipWith (\_ x -> x)

windows' n = map (take n) . tails

windows n xs = takeLengthOf (drop (n - 1) xs) (windows' n xs)

main = do
  ip <- lines <$> getContents
  -- let ip = lines input
  let matrix = map parseLine ip
  let indexes = [[(i, j) | j <- [0 ..]] | i <- [0 ..]]
  let f = fmap zip indexes
  let resultMatrix = zipWith ($) f matrix
  let graphResult = evalState (getResults resultMatrix) []
  let sol2 = levels <$> graphResult
  let onlySymbol = map (filter (\x -> case snd x of Symbol _ -> False; _ -> True) . flatten) graphResult
  let result = nub $ sort $ concat onlySymbol
  print $ sum $ concat $ helperFunction result
  let onlyStar = filter ((\x -> case snd x of Symbol '*' -> True; _ -> False) . head . head) sol2
  let gearNumbers = map ((helperFunction . nub . sort . concat) . tail) onlyStar
  let gears = map concat $ filter (\x -> length x > 1 || length (head x) > 1) gearNumbers
  let gearCombo = map (map product . windows 2) gears
  print $ sum $ concat gearCombo

helperFunction result =
  let fn :: MatrixElement -> [[MatrixElement]] -> [[MatrixElement]]
      fn element ls@([] : lsts) = [element] : lsts
      fn element ls@(lst@(x : _) : lsts) =
        let lastY = snd $ fst x
            y = snd $ fst element
         in if abs (lastY - y) == 1 then (element : lst) : lsts else [element] : ls
      fn' :: [Int] -> Int
      fn' = foldl (\x y -> x * 10 + y) 0
      res' = groupBy (\x y -> fst (fst x) == fst (fst y)) result
      res'' = map (foldr fn [[]]) res'
      res''' = (map . map . map) (\x -> case snd x of (Element i) -> i) res''
   in (map . map) fn' res'''

getResults :: [[MatrixElement]] -> State [(Int, Int)] [Tree MatrixElement]
getResults matrix = unfoldForestM_BF f $ filter (\x -> case snd x of Symbol _ -> True; _ -> False) $ concat matrix
  where
    f node = do
      let (coord@(x, y), value) = node
      visited <- get
      if coord `elem` visited
        then return (node, [])
        else do
          put (coord : visited)
          let nextNodes = neighbors matrix coord
          return (node, nextNodes)

neighbors :: [[MatrixElement]] -> Coord -> [MatrixElement]
neighbors matrix (x, y) =
  let d = [-1, 0, 1]
   in nub $ do
        dy <- d
        dx <- d
        let newx = x + dx
        let newy = y + dy
        guard $ not (newx == x && newy == y)
        guard (newx >= 0 && newx < length matrix)
        guard (newy >= 0 && newy < length (head matrix))
        let v = matrix !! newx !! newy
        case snd v of
          Element i -> return v
          _ -> []

parseLine :: String -> [Element]
parseLine string =
  fromRight [] $
    P.runParser
      do
        P.many $
          P.choice
            [ Element . read . printf "%c" <$> P.digit,
              do
                _ <- P.string "."
                return None,
              do
                Symbol <$> P.anyChar
            ]
      ()
      ""
      string
