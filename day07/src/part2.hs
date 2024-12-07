main :: IO ()
main = readFile "input.txt" >>= print . part2 . map parseLine . lines

part2 :: [[Int]] -> Int
part2 = sum . map head . filter isEquationValid

isEquationValid :: [Int] -> Bool
isEquationValid []      = False
isEquationValid [_]     = False
isEquationValid (x:xs)  = canMakeTarget xs x
  where
    canMakeTarget :: [Int] -> Int -> Bool
    canMakeTarget [] _          = False
    canMakeTarget [n] target    = n == target
    canMakeTarget (a:b:rest) t  =
      let results = [a+b, a*b, concatNums a b]
      in any (\r -> r <= t && canMakeTarget (r:rest) t) results

concatNums :: Int -> Int -> Int
concatNums a b = read (show a ++ show b)

parseLine :: String -> [Int]
parseLine line =
  case break (== ':') line of
    (start, _:rest) -> read start : map read (words rest)
    _               -> error "Invalid input format"
