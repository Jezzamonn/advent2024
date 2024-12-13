import Data.List (sort)
import Data.Ix (Ix(inRange))

deltas :: [Int] -> [Int]
deltas x = zipWith (-) (tail x) x

deltasNotTooBig :: [Int] -> Bool
deltasNotTooBig = all (inRange (1, 3) . abs)

increasingOrDecreasing :: [Int] -> Bool
increasingOrDecreasing x =
    let signs = map signum x
        start = head signs
    in all (== start) signs

reportIsSafe :: [Int] -> Bool
reportIsSafe x =
    let d = deltas x
    in increasingOrDecreasing d && deltasNotTooBig d

removeElemAt :: Int -> [a] -> [a]
removeElemAt i xs =
    let (ys, _:zs) = splitAt i xs
    in ys ++ zs

makeSubReports :: [Int] -> [[Int]]
makeSubReports x =
    let indices = [0..length x - 1]
    in map (`removeElemAt` x) indices

isSafeWithRemoval :: [Int] -> Bool
isSafeWithRemoval x = any reportIsSafe (makeSubReports x)

solve :: String -> Int
solve contents =
    let allLines = lines contents
        reports = map (map read . words) allLines :: [[Int]]
    in
        length $ filter isSafeWithRemoval reports

main :: IO ()
main = do
    contents <- getContents
    print $ solve contents
