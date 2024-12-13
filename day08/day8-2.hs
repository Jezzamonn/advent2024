
import qualified Data.Set as Set
import qualified Data.Map as Map
import Grid (addIndex2D, inBounds, set2D)
import Data.Tuple (swap)

pairAnodePositions :: [[a]] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
pairAnodePositions grid p1 p2 = (pairAnodePositionsOneDir grid p1 p2) ++ (pairAnodePositionsOneDir grid p2 p1)

pairAnodePositionsOneDir :: [[a]] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
pairAnodePositionsOneDir grid (x1, y1) (x2, y2) =
    let dx = x2 - x1
        dy = y2 - y1
    in takeWhile (inBounds grid) $ map (\i -> (x2 + dx * i, y2 + dy * i)) [0..]

allPairs :: Eq a => [a] -> [(a, a)]
allPairs xs = [(x, y) | x <- xs, y <- xs, x /= y]

allAnodePositions :: [[a]] -> [(Int, Int)] -> [(Int, Int)]
allAnodePositions grid positions = concatMap (uncurry $ pairAnodePositions grid) $ allPairs positions


main :: IO ()
main = do
    contents <- getContents
    let grid = lines contents
        gridWithIndex = addIndex2D grid
        nonDotCharsAndPos = concatMap (filter (\(ix, c) -> c /= '.')) gridWithIndex
        charsToPos :: Map.Map Char [(Int, Int)]
        charsToPos = Map.fromListWith (++) $ map (\(ix, c) -> (c, [ix])) nonDotCharsAndPos
    -- print charsToPos
    let charsToAnodePos = Map.map (allAnodePositions grid) charsToPos
    let allAnodePositions :: Set.Set (Int, Int)
        allAnodePositions = Set.fromList $ filter (inBounds grid) $ concat $ Map.elems charsToAnodePos
    print $ length allAnodePositions
    let updatedGrid = foldl (\g p -> set2D g p '#') grid $ Set.toList allAnodePositions
    putStrLn $ unlines updatedGrid
    return ()
