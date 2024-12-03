import Text.Regex.PCRE

mulRegex :: String
mulRegex = "(mul)\\((\\d{1,3}),(\\d{1,3})\\)"
doRegex :: String
doRegex = "(do)\\(\\)"
dontRegex :: String
dontRegex = "(don't)\\(\\)"
regex :: String
regex = mulRegex ++ "|" ++ doRegex ++ "|" ++ dontRegex

extractFunctions :: String -> [[String]]
extractFunctions x = x =~ regex :: [[String]]

data InstructionState = InstructionState {
    accumulator :: Int,
    enabled :: Bool
} deriving (Show)

handleInstruction :: InstructionState -> [String] -> InstructionState
handleInstruction state [_, "mul", x, y, _, _]
    | enabled state = InstructionState {
        accumulator = accumulator state + read x * read y,
        enabled = enabled state
    }
    | otherwise = state
handleInstruction state [_, _, _, _, "do", _] = InstructionState {
    accumulator = accumulator state,
    enabled = True
}
handleInstruction state [_, _, _, _, _, "don't"] = InstructionState {
    accumulator = accumulator state,
    enabled = False
}
handleInstruction _ instruction = error $ "Unknown instruction: " ++ show instruction

handleInstructions :: [[String]] -> InstructionState
handleInstructions = foldl handleInstruction InstructionState {accumulator = 0, enabled = True}

solve :: String -> Int
solve contents = accumulator $ handleInstructions $ extractFunctions contents

main :: IO ()
main = do
    contents <- getContents
    print $ solve contents