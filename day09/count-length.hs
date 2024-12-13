sumLength :: String -> Int
sumLength contents = sum $ map (read . (:[])) contents

main :: IO ()
main = do
    contents <- getContents
    print $ sumLength contents