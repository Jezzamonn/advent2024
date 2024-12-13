import Data.List (sort)
import Data.Ix (Ix(inRange))

solve :: String -> Int
solve contents =
    let allLines = lines contents
        nums = map (map read . words) allLines :: [[Int]]
        deltas = map (\x -> zipWith (-) (tail x) x) nums
        deltasNotTooBig :: [Int] -> Bool
        deltasNotTooBig = all (inRange (1, 3) . abs)
        increasingOrDecreasing :: [Int] -> Bool
        increasingOrDecreasing x =
            let signs = map signum x
                start = head signs
            in all (== start) signs
    in
        length . filter increasingOrDecreasing . filter deltasNotTooBig $ deltas


main :: IO ()
main = do
    contents <- getContents
    print $ solve contents
