{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (guard)
import Control.Monad.State (State, evalState, get, put)
import Data.Char (digitToInt)
import Data.Either (fromRight)
import Data.Foldable (Foldable (foldl'))
import Data.List (delete, nub, sort, sortBy)
import Data.List.Split (chunksOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.Tree (Tree, drawTree, flatten, unfoldTreeM)
import Debug.Trace (trace)
import Text.Parsec qualified as P
import Text.Printf (printf)
import Text.RawString.QQ (r)

input =
  [r|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4|]

data Entity x = Seed x | Soil x | Fertilizer x | Water x | Light x | Temeprature x | Humidity x | Location x deriving (Show, Eq, Ord, Functor)

type IntEntity = Entity Int

type EntityRange = Entity (Int, Int)

type EntityMap = Map.Map EntityRange EntityRange

type TransitionFn = Map.Map IntEntity (Int -> IntEntity)

data Data = Data {seeds :: [IntEntity], maps :: EntityMap, transitions :: TransitionFn}

getFromEntity :: Entity x -> x
getFromEntity (Seed x) = x
getFromEntity (Soil x) = x
getFromEntity (Fertilizer x) = x
getFromEntity (Water x) = x
getFromEntity (Light x) = x
getFromEntity (Temeprature x) = x
getFromEntity (Humidity x) = x
getFromEntity (Location x) = x

default_ = Data {seeds = [], maps = Map.empty, transitions = Map.empty}

main = do
  ip <- getContents
  -- let ip = input
  let r = fromRight default_ $ P.runParser parseData () "" ip
  let result1 = map getFromEntity $ toLocation r.maps r.transitions <$> r.seeds
  -- print $ minimum result1
  let partioned = chunksOf 2 r.seeds
  let input = map (\x -> Seed (getFromEntity $ head x, getFromEntity (head x) + getFromEntity (last x))) partioned
  let res inp = filter (\x -> case x of (Location _) -> True; _ -> False) $ flatten $ evalState (locationsFn r.maps r.transitions inp) []
  print $ fst $ getFromEntity $ minimum $ concatMap (res . entityToRange) r.seeds
  print $ fst $ getFromEntity $ minimum $ concatMap res input

parseData = do
  P.string "seeds:"
  P.spaces
  seeds <- P.many1 parseInteger
  r <- P.many1 parseMaps
  let ms = foldl' Map.union Map.empty $ fst <$> r
  let ts = foldl' Map.union Map.empty $ snd <$> r
  return $ Data {seeds = Seed <$> seeds, maps = ms, transitions = ts}

parseInteger = do
  digits <- P.many1 P.digit
  P.optional P.spaces
  return $ read digits

parseMaps = do
  source <- P.many1 P.alphaNum
  P.string "-to-"
  destination <- P.many1 P.alphaNum
  let sourceFn = stringToFn source
  let destinationFn = stringToFn destination
  P.spaces
  P.string "map"
  P.string ":"
  P.spaces
  r <- P.many1 $ do
    destStart <- parseInteger
    sourceStart <- parseInteger
    length <- parseInteger
    P.spaces
    let map = Map.fromList [(sourceFn (sourceStart, sourceStart + length), destinationFn (destStart, destStart + length))]
    return map
  let unifiedMap = foldl' Map.union Map.empty r
  return (unifiedMap, Map.fromList [(sourceFn minBound, destinationFn)])

stringToFn string
  | string == "seed" = Seed
  | string == "soil" = Soil
  | string == "fertilizer" = Fertilizer
  | string == "water" = Water
  | string == "light" = Light
  | string == "temperature" = Temeprature
  | string == "humidity" = Humidity
  | string == "location" = Location
  | otherwise = undefined

intersect :: EntityRange -> EntityRange -> [Maybe EntityRange]
intersect (Seed x) (Seed y) = fmap Seed <$> intersect' x y
intersect (Soil x) (Soil y) = fmap Soil <$> intersect' x y
intersect (Fertilizer x) (Fertilizer y) = fmap Fertilizer <$> intersect' x y
intersect (Water x) (Water y) = fmap Water <$> intersect' x y
intersect (Light x) (Light y) = fmap Light <$> intersect' x y
intersect (Temeprature x) (Temeprature y) = fmap Temeprature <$> intersect' x y
intersect (Humidity x) (Humidity y) = fmap Humidity <$> intersect' x y
intersect (Location x) (Location y) = fmap Location <$> intersect' x y
intersect _ _ = [Nothing]

fromRange l r = if l < r then Just (l, r) else Nothing

intersect' x y = if x < y then intersect'' x y else intersect'' y x

doesIntersect :: (Int, Int) -> (Int, Int) -> Bool
doesIntersect x y = if x < y then doesIntersect'' x y else doesIntersect'' y x

doesIntersect'' x@(x1, x2) y@(y1, y2) = (x2 - 1) >= y1 && (y2 - 1) >= x1

intersect'' x y =
  let a = fst x
      b = snd x
      c = fst y
      d = snd y
   in if b < c then [] else filter isJust $ nub [fromRange (min a c) (min b c), fromRange (min b c) (min b d), fromRange (min b d) (max b d)]

entityToRange :: IntEntity -> EntityRange
entityToRange e = (\x -> (x, x + 1)) <$> e

toLocation :: EntityMap -> TransitionFn -> IntEntity -> IntEntity
toLocation m ts s@(Location x) = s
toLocation m ts s =
  let keyFn k v =
        let start = fst <$> k
            end = snd <$> k
         in start <= s && s < end
      m' = Map.filterWithKey keyFn m
      size = Map.size m'
   in if size == 0
        then
          let x = minBound <$ s
              fn = ts Map.! x
           in toLocation m ts (fn $ getFromEntity s)
        else
          let k = head $ Map.keys m'
              offset = getFromEntity s - getFromEntity (fst <$> k)
              v = (m' Map.! k)
           in toLocation m ts ((+ offset) . fst <$> v)

filterMap :: EntityMap -> EntityRange -> EntityMap
filterMap m s =
  let keyFn k v = any isJust (k `intersect` s)
      m' = Map.filterWithKey keyFn m
   in m'

locationsFn :: EntityMap -> TransitionFn -> EntityRange -> State [EntityRange] (Tree EntityRange)
locationsFn m ts = unfoldTreeM f
  where
    f :: EntityRange -> State [EntityRange] (EntityRange, [EntityRange])
    f e =
      let m' = filterMap m e
          k' = minBound <$ e
          v = m' Map.!? e
          fn = ts Map.! k'
          keys = Map.keys m'
          default_ =
            let first = getFromEntity $ fst <$> e
                second = getFromEntity $ snd <$> e
             in (,second) <$> fn first
          results' = do
            key <- keys
            guard $ doesIntersect (getFromEntity key) (getFromEntity e)
            let value = m' Map.!? key
            let intersection = delete e $ nub $ sort ((<$ e) <$> filter (doesIntersect (getFromEntity e)) (getFromEntity . fromJust <$> intersect key e))
            let result = case value of
                  (Just v) ->
                    let diff = getFromEntity (fst <$> v) - getFromEntity (fst <$> key)
                        first = getFromEntity $ fst <$> e
                        second = getFromEntity $ snd <$> e
                     in (first + diff, second + diff) <$ v
                  _ -> default_
            if null intersection then return result else intersection
          results = if null results' then [default_] else results'
       in do
            visited <- get
            if e `elem` visited
              then return (e, [])
              else do
                put (e : visited)
                return $ case e of
                  (Location _) -> (e, [])
                  _ -> (e, results)
