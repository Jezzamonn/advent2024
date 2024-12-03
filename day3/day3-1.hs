import Text.Regex.PCRE

regex = "mul\\((\\d{1,3}),(\\d{1,3})\\)"

fetchMulInstructionParameters :: String -> [(Int, Int)]
fetchMulInstructionParameters input =
    let matches = input =~ regex :: [[String]]
    in map (\[_, x, y] -> (read x, read y)) matches

mulTuple :: (Int, Int) -> Int
mulTuple (x, y) = x * y

solve :: String -> Int
solve contents = sum $ map mulTuple (fetchMulInstructionParameters contents)


main :: IO ()
main = do
    contents <- getContents
    print $ solve contents