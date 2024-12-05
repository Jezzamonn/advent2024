import Data.List (transpose, elemIndices)
import Data.Maybe (catMaybes, mapMaybe)

inBounds :: [[a]] -> (Int, Int) -> Bool
inBounds xs (y, x) = x >= 0 && y >= 0 && y < length xs && x < length (head xs)

get2D :: [[a]] -> (Int, Int) -> Maybe a
get2D xs (y, x) = if inBounds xs (y, x)
    then Just (xs !! y !! x)
    else Nothing

elemIndices2D :: Eq a => a -> [[a]] -> [(Int, Int)]
elemIndices2D elem xs =
    let colIndices = map (elemIndices elem) xs
        tupleIndices :: Int -> [Int] -> [(Int, Int)]
        tupleIndices x = map (x,)
        rowIndices = zipWith tupleIndices [0..] colIndices
    in concat rowIndices

xShapedNeighbors :: (Int, Int) -> [(Int, Int)]
-- Order is important.
xShapedNeighbors (y, x) = [(y - 1, x - 1), (y - 1, x + 1), (y + 1, x + 1), (y + 1, x - 1)]

isMasNeighbors :: String -> Bool
isMasNeighbors "MMSS" = True
isMasNeighbors "MSSM" = True
isMasNeighbors "SSMM" = True
isMasNeighbors "SMMS" = True
isMasNeighbors _ = False

countXmas :: [[Char]] -> Int
countXmas grid =
    let aIndices = elemIndices2D 'A' grid
        neighborIndices = map xShapedNeighbors aIndices
        neighbors = map (mapMaybe (get2D grid)) neighborIndices
        masNeighbors = filter isMasNeighbors neighbors
    in length masNeighbors


main :: IO ()
main = do
    contents <- getContents
    print $ countXmas (lines contents)
