import Direction (Direction(..), turnRight, delta)
import Grid (inBounds, get2D, find2D, set2D)

nextPos :: (Int, Int) -> Direction -> [[Char]] -> Maybe ((Int, Int), Direction)
nextPos p dir grid =
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

markReachableSpaces :: (Int, Int) -> Direction -> [[Char]] -> [[Char]]
markReachableSpaces start dir grid =
    let nextGrid = set2D grid start 'X'
        maybeNext = nextPos start dir grid
    in case maybeNext of
        Just (next, nextDir) -> markReachableSpaces next nextDir nextGrid
        Nothing -> nextGrid

-- Counts 'X's
countReachedSpaces :: [[Char]] -> Int
countReachedSpaces grid = length $ concatMap (filter (== 'X')) grid

solvePt1 :: String -> Int
solvePt1 contents =
    let grid = lines contents
        maybeStart = find2D grid '^'
    in case maybeStart of
        Just start -> countReachedSpaces $ markReachableSpaces start Up grid
        Nothing -> error "No starting position found"

main :: IO ()
main = do
    contents <- getContents
    print $ solvePt1 contents
