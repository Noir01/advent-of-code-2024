main :: IO ()
main = readFile "input.txt" >>= print . part1 . parseContent

part1 :: [[Int]] -> Int
part1 = length . filter isReportSafe

isReportSafe :: [Int] -> Bool
isReportSafe xs = (isIncreasing xs || isDecreasing xs) && adjacentDifferenceLessThan4 xs
  where
    isIncreasing ys = and $ zipWith (<) ys (tail ys)
    isDecreasing ys = and $ zipWith (>) ys (tail ys)
    adjacentDifferenceLessThan4 ys = all ((< 4) . abs . uncurry subtract) (zip ys (tail ys))

parseContent :: String -> [[Int]]
parseContent = map (map read . words) . lines