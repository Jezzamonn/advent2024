
import qualified Data.Set as Set
import qualified Data.Map as Map
import Grid (addIndex2D, inBounds)
import Data.Tuple (swap)

pairAnnodePositions :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
pairAnnodePositions (x1, y1) (x2, y2) =
    let dx = x2 - x1
        dy = y2 - y1
    in [(x2 + dx, y2 + dy), (x1 - dx, y1 - dy)]

allPairs :: Eq a => [a] -> [(a, a)]
allPairs xs = [(x, y) | x <- xs, y <- xs, x /= y]

allAnodePositions :: [(Int, Int)] -> [(Int, Int)]
allAnodePositions positions = concatMap (uncurry pairAnnodePositions) $ allPairs positions


main :: IO ()
main = do
    contents <- getContents
    let grid = lines contents
        gridWithIndex = addIndex2D grid
        nonDotCharsAndPos = concatMap (filter (\(ix, c) -> c /= '.')) gridWithIndex
        charsToPos :: Map.Map Char [(Int, Int)]
        charsToPos = Map.fromListWith (++) $ map (\(ix, c) -> (c, [ix])) nonDotCharsAndPos
    print charsToPos
    let charsToAnodePos = Map.map allAnodePositions charsToPos
    let allAnodePositions :: Set.Set (Int, Int)
        allAnodePositions = Set.fromList $ concat $ Map.elems charsToAnodePos
    print $ length $ filter (inBounds grid) $ Set.toList allAnodePositions
    return ()
