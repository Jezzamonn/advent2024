module Grid (
    inBounds,
    get2D,
    set2D,
    addIndex2D,
    find2D,
    index2D
) where

import Data.List (find)

inBounds :: [[a]] -> (Int, Int) -> Bool
inBounds grid (x, y) = x >= 0 && y >= 0 && y < length grid && x < length (head grid)

get2D :: [[a]] -> (Int, Int) -> Maybe a
get2D grid (x, y) = if inBounds grid (x, y)
    then Just (grid !! y !! x)
    else Nothing

set2D :: [[a]] -> (Int, Int) -> a -> [[a]]
set2D grid (x, y) val = if inBounds grid (x, y)
    then
        let (beforeRows,row:afterRows) = splitAt y grid
            (beforeElems,_:afterElems) = splitAt x row
            newRow = beforeElems ++ val : afterElems
        in beforeRows ++ newRow : afterRows
    else error $ "Out of bounds: (" ++ show x ++ "," ++ show y ++ ")"

addIndex :: [a] -> [(Int,a)]
addIndex = zip [0..]

addIndex2D :: [[a]] -> [[((Int,Int), a)]]
addIndex2D grid =
    let gridWithColIndex = map addIndex grid
        gridWithRowAndColIndex = addIndex gridWithColIndex
    in map packIndex gridWithRowAndColIndex
    where packIndex :: (Int, [(Int, a)]) -> [((Int, Int), a)]
          packIndex (rowIndex, row) = map (\(colIndex, elem) -> ((colIndex, rowIndex), elem)) row

index2D :: [[a]] -> [(Int, Int)]
index2D grid = [(x, y) | y <- [0..length grid - 1], x <- [0..length (head grid) - 1]]

find2D :: Eq a => [[a]] -> a -> Maybe (Int, Int)
find2D grid val =
    let gridWithIndex = addIndex2D grid
        flatGrid = concat gridWithIndex
        match = find ((== val) . snd) flatGrid
    in fmap fst match
