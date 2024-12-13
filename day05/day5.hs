{-# LANGUAGE TupleSections #-}

import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import Data.List (sortBy)

readPairs :: String -> [(Int, Int)]
readPairs =
    map (listToTuple . splitOn "|") . lines
    where listToTuple :: [String] -> (Int, Int)
          listToTuple [a, b] = (read a, read b)
          listToTuple x = error ("Invalid pair: " ++ show x)


makeOrderingMap :: String -> Map.Map (Int, Int) Ordering
makeOrderingMap start =
    let pairs = readPairs start
        revPairs = map swap pairs
    in Map.fromList (map (, LT) pairs ++ map (, GT) revPairs)

isSorted :: Map.Map (Int, Int) Ordering -> [Int] -> Bool
isSorted _ [] = True
isSorted _ [_] = True
isSorted orderingMap (x:y:xs) =
    case Map.lookup (x, y) orderingMap of
        Just LT -> isSorted orderingMap (y:xs)
        Just GT -> False
        _ -> error ("No ordering for " ++ show (x, y))

getMiddle :: [a] -> a
getMiddle xs = xs !! (length xs `div` 2)


sortByMap :: Map.Map (Int, Int) Ordering -> [Int] -> [Int]
sortByMap orderingMap = sortBy (\a b -> Map.findWithDefault EQ (a, b) orderingMap)

main :: IO ()
main = do
    contents <- getContents
    let [orders, lists] = splitOn "\n\n" contents
        orderingMap = makeOrderingMap orders
        lists' = map (map read . splitOn ",") (lines lists) :: [[Int]]
        -- sortedLists = filter (isSorted orderingMap) lists'
        unsortedLists = filter (not . isSorted orderingMap) lists'
        resortedLists = map (sortByMap orderingMap) unsortedLists
    print $ sum $ map getMiddle resortedLists
    -- print resortedLists
