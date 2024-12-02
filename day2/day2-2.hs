import Data.List (sort)
import Data.Ix (Ix(inRange))

deltasNotTooBig :: [Int] -> Bool
deltasNotTooBig = all (inRange (1, 3) . abs)

increasingOrDecreasing :: [Int] -> Bool
increasingOrDecreasing x =
    let signs = map signum x
        start = head signs
    in all (== start) signs

reportIsSafe :: [Int] -> Bool
reportIsSafe x = increasingOrDecreasing x && deltasNotTooBig x

removeElemAt :: Int -> [a] -> [a]
removeElemAt i xs =
    let (ys, _:zs) = splitAt i xs
    in ys ++ zs

makeSubReports :: [Int] -> [[Int]]
makeSubReports x =
    let indices = [0..length x - 1]
    in map (`removeElemAt` x) indices

deltas :: [Int] -> [Int]
deltas x = zipWith (-) (tail x) x

solve :: String -> Int
solve contents =
    let allLines = lines contents
        reports = map (map read . words) allLines :: [[Int]]
        subReports = map makeSubReports reports
        subReportDeltas = map (map deltas) subReports
        subReportsAreSafe = map (map reportIsSafe) subReportDeltas
    in
        length $ filter or subReportsAreSafe

main :: IO ()
main = do
    contents <- getContents
    print $ solve contents
