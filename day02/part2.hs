main :: IO ()
main = readFile "input.txt" >>= print . part2 . parseContent

part2 :: [[Int]] -> Int
part2 = length . filter isReportTolerablySafe

isReportSafe :: [Int] -> Bool
isReportSafe xs = (isIncreasing xs || isDecreasing xs) && adjacentDifferenceLessThan4 xs
  where
    isIncreasing ys = and $ zipWith (<) ys (tail ys)
    isDecreasing ys = and $ zipWith (>) ys (tail ys)
    adjacentDifferenceLessThan4 ys = all ((< 4) . abs . uncurry subtract) $ zip ys (tail ys)

isReportTolerablySafe :: [Int] -> Bool
isReportTolerablySafe report =
  isReportSafe report || any (isReportSafe . removeAt report) [0 .. length report - 1]

removeAt :: [a] -> Int -> [a]
removeAt xs i = take i xs ++ drop (i + 1) xs

parseContent :: String -> [[Int]]
parseContent = map (map read . words) . lines