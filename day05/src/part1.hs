import Data.Array
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (arr, xss) = parseInput input
  print $ part1 arr xss

part1 :: Array Int [Int] -> [[Int]] -> Int
part1 arr xss =
    sum (map checkOne xss)
  where
    checkOne xs =
      let pairs = adjacentPairs xs
      in if all (\(a,b) -> b `elem` (arr ! a)) pairs
         then getMiddleElement xs
         else 0

adjacentPairs :: [Int] -> [(Int, Int)]
adjacentPairs []       = []
adjacentPairs [_]      = []
adjacentPairs (x:y:xs) = (x, y) : adjacentPairs (y:xs)


getMiddleElement :: [Int] -> Int
getMiddleElement arr = if not (null arr) then arr !! (length arr `div` 2) else 0

parseInput :: String -> (Array Int [Int], [[Int]])
parseInput input =
  let
    pageRules = array (1, 100) [(i, []) | i <- [1..100]]

    insertValue :: Array Int [Int] -> Int -> Int -> Array Int [Int]
    insertValue arr key value =
        let existingValues = arr ! key
        in arr // [(key, value : existingValues)]

    parseLine :: (Array Int [Int], [[Int]]) -> String -> (Array Int [Int], [[Int]])
    parseLine (currentRules, pages) line
      | null line = (currentRules, pages)
      | '|' `elem` line =
        let parts = splitOn "|" line
            (keyStr, valueStr) = case parts of
                [k, v] -> (k, v)
                _      -> error "Invalid input format"
            key   = read keyStr   :: Int
            value = read valueStr :: Int
            newRules = insertValue currentRules key value
        in (newRules, pages)
      | otherwise =
        let page = map read (splitOn "," line) :: [Int]
        in (currentRules, page : pages)


  in foldl parseLine (pageRules, [[]]) (lines input)