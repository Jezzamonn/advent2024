import Data.List (transpose)

inBounds :: [[a]] -> (Int, Int) -> Bool
inBounds xs (y, x) = x >= 0 && y >= 0 && y < length xs && x < length (head xs)

indicesInDirection :: [[a]] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
indicesInDirection xs (y , x) (dy, dx) =
    let next = (y + dy, x + dx)
    in if inBounds xs (y, x)
        then (y, x) : indicesInDirection xs next (dy, dx)
        else []

positiveDiagonalIndices :: [[a]] -> [[(Int, Int)]]
positiveDiagonalIndices xs =
    let height = length xs
        width = length (head xs)
        left = [(y, 0) | y <- [height - 1, height - 2..1]]
        top = [(0, x) | x <- [0..width - 1]]
        starts = left ++ top
    in map (\start -> indicesInDirection xs start (1, 1)) starts

negativeDiagonalIndices :: [[a]] -> [[(Int, Int)]]
negativeDiagonalIndices xs =
    let height = length xs
        width = length (head xs)
        right = [(y, width - 1) | y <- [height - 1, height - 2..1]]
        top = [(0, x) | x <- [width - 1, width - 2..0]]
        starts = right ++ top
    in map (\start -> indicesInDirection xs start (1, -1)) starts

countXmas :: String -> Int
countXmas [] = 0
countXmas ('X':'M':'A':'S':xs) = 1 + countXmas xs
countXmas (_:xs) = countXmas xs

countXmasBothDirections :: String -> Int
countXmasBothDirections xs = countXmas xs + countXmas (reverse xs)

countXmasRows :: [String] -> Int
countXmasRows = sum . map countXmasBothDirections

countXmasCols :: [String] -> Int
countXmasCols = countXmasRows . transpose

countXmasPosDiagonals :: [String] -> Int
countXmasPosDiagonals xs =
    let indices = positiveDiagonalIndices xs
        diags = map (map (\(y, x) -> xs !! y !! x)) indices
    in countXmasRows diags

countXmasNegDiagonals :: [String] -> Int
countXmasNegDiagonals xs =
    let indices = negativeDiagonalIndices xs
        diags = map (map (\(y, x) -> xs !! y !! x)) indices
    in countXmasRows diags

countAllXmas :: [String] -> Int
countAllXmas xs = sum [countXmasRows xs, countXmasCols xs, countXmasPosDiagonals xs, countXmasNegDiagonals xs]

main :: IO ()
main = do
    contents <- getContents
    print $ countAllXmas (lines contents)
