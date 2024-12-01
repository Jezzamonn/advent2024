main :: IO ()
main = do
    contents <- getContents
    let lineCount = length (lines contents)
    print lineCount
