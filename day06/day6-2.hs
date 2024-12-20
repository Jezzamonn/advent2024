import Direction (Direction(..), turnRight, delta)
import Grid (inBounds, get2D, find2D, set2D, index2D)
import qualified Data.Set as Set

type Grid = [[Char]]
type Pos = (Int, Int)
type PosDir = (Pos, Direction)

nextPos :: PosDir -> Grid -> Maybe PosDir
nextPos (p, dir) grid =
    let p' = moveInDir p dir
        nextSquare = get2D grid p'
        rightDir = turnRight dir
    in case nextSquare of
        Just '#' -> Just (moveInDir p rightDir, rightDir)
        Just _ -> Just (p', dir)
        Nothing -> Nothing

moveInDir :: (Int, Int) -> Direction -> (Int, Int)
moveInDir (x, y) dir =
    let (dx, dy) = delta dir
    in (x + dx, y + dy)

getReachablePosAndDirs :: PosDir -> Grid -> [PosDir] -> [PosDir]
getReachablePosAndDirs posDir grid visited =
    let maybeNext = nextPos posDir grid
    in case maybeNext of
        Just next -> getReachablePosAndDirs next grid (next : visited)
        Nothing -> visited

isLoop :: PosDir -> Grid -> [PosDir] -> Bool
isLoop posDir grid visited =
    let next = nextPos posDir grid
    in case next of
        Just nextPosDir -> (nextPosDir `elem` visited) || isLoop nextPosDir grid (nextPosDir : visited)
        Nothing -> False

changingPointMakesLoop :: PosDir -> Grid -> Pos -> Bool
changingPointMakesLoop posDir grid p =
    let existing = get2D grid p
        newGrid = set2D grid p '#'
    in case existing of
        Just '#' -> False
        Just _ -> isLoop posDir newGrid [posDir]
        Nothing -> False

-- Counts 'X's
countReachedSpaces :: Grid -> Int
countReachedSpaces grid = length $ concatMap (filter (== 'X')) grid

main :: IO ()
main = do
    contents <- getContents
    let grid = lines contents
        gridIndices = index2D grid
        maybeStart = find2D grid '^'
        startPosDir = case maybeStart of
            Just start -> (start, Up)
            Nothing -> error "No starting position found"
        reachablePosDirs = getReachablePosAndDirs startPosDir grid []
        allPos = Set.fromList $ map fst reachablePosDirs
        allPosButStart = Set.delete (fst startPosDir) allPos
    print $ length $ filter (changingPointMakesLoop startPosDir grid) (Set.toList allPosButStart)
