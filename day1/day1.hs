import Data.List (sort)

solve :: String -> Int
solve contents =
    let allLines = lines contents
        allWords = map words allLines
        firstCol = map (read . (!! 0)) allWords :: [Int]
        secondCol = map (read . (!! 1)) allWords :: [Int]
        absDiff = zipWith (\x y -> abs(x - y)) (sort firstCol) (sort secondCol)
    in sum absDiff


main :: IO ()
main = do
    contents <- getContents
    print $ solve contents
