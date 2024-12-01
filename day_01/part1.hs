import Data.List (sort)

main :: IO ()
main = readFile "input.txt" >>= print . part1 . map parseLine . lines

part1 :: [(Int, Int)] -> Int
part1 pairs = sum $ zipWith (\x y -> abs (x - y)) (sort as) (sort bs)
    where (as, bs) = unzip pairs

parseLine :: String -> (Int, Int)
parseLine = (\[x, y] -> (x, y)) . map read . words