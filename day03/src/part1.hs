import Text.Regex.TDFA ((=~))

-- | Extract all "mul(X,Y)" patterns and return the numbers X and Y as tuples
extractMulNumbers :: String -> [(Int, Int)]
extractMulNumbers input =
  let -- Regex pattern for capturing X and Y inside "mul(X,Y)"
      pattern = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"
      matches = input =~ pattern :: [[String]]
  in map extractNumbers matches
  where
    -- Extract X and Y from the match groups
    extractNumbers :: [String] -> (Int, Int)
    extractNumbers (_:x:y:_) = (read x, read y)
    extractMulNumbers _  = error

main :: IO ()
main = readFile "input.txt" >>= print . part1 . lines

part1 :: [String] -> Int
part1 [] = 0
part1 (x:xs) = 
  let nums = extractMulNumbers x
  in sum (map (\(a, b) -> a * b) nums) + part1 xs