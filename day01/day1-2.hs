import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

solve :: String -> Int
solve contents =
    let allLines = lines contents
        allWords = map words allLines
        firstCol = map (read . (!! 0)) allWords :: [Int]
        secondCol = map (read . (!! 1)) allWords :: [Int]
        -- Count the number of each element in the second column, in a Map.
        secondColCounts = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty secondCol
        similarityScore x = fromMaybe 0 (Map.lookup x secondColCounts) * x
    in sum $ map similarityScore firstCol


main :: IO ()
main = do
    contents <- getContents
    print $ solve contents
