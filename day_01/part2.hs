import Data.List (sort)
import qualified Data.Map.Strict as Map

main :: IO ()
main = readFile "input.txt" >>= print . part2 . map parseLine . lines

part2 :: [(Int, Int)] -> Int
part2 pairs = sum [ a * Map.findWithDefault 0 a countsMap | a <- as ]
  where
    (as, bs) = unzip pairs
    countsMap = countOccurrences bs

parseLine :: String -> (Int, Int)
parseLine = (\[x, y] -> (x, y)) . map read . words

countOccurrences :: (Ord a) => [a] -> Map.Map a Int
countOccurrences = Map.fromListWith (+) . flip zip (repeat 1)