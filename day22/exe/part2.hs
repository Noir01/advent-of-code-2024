module Main where

import Data.Bits (shiftL, shiftR, xor, (.&.))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (tails)

main :: IO ()
main = readFile "input.txt" >>= print . part2 . parseContent

part2 :: [Int] -> Int
part2 secretNumbers = maximum (M.elems patterns)
  where
    allSecretNumbers = map (applyNTimes nextSecretNumber 2000) secretNumbers -- [[Int]]
    prices = map (map onesPlace) allSecretNumbers -- [[Int]]
    allChanges = map generateChanges prices -- [[Int]]
    patterns = foldl collectPatterns M.empty (zip allChanges prices)

collectPatterns :: M.Map [Int] Int -> ([Int], [Int]) -> M.Map [Int] Int
collectPatterns acc (diffs, nums) = go acc S.empty (zip (tails diffs) (drop 4 nums))
  where
    go m _ [] = m
    go m seen ((pat, n) : xs)
      | length pat >= 4 =
          let pat4 = take 4 pat
          in if pat4 `S.member` seen
               then go m seen xs
               else go (M.insertWith (+) pat4 (onesPlace n) m) (S.insert pat4 seen) xs
      | otherwise = m

onesPlace :: Int -> Int
onesPlace x = x `mod` 10

applyNTimes :: (Int -> Int) -> Int -> Int -> [Int]
applyNTimes f n x = take (n + 1) $ iterate f x

nextSecretNumber :: Int -> Int
nextSecretNumber currentNumber = ((b `shiftL` 11) `xor` b) .&. 0xFFFFFF
  where
    a = ((currentNumber `shiftL` 6) `xor` currentNumber) .&. 0xFFFFFF
    b = ((a `shiftR` 5) `xor` a) .&. 0xFFFFFF

generateChanges :: [Int] -> [Int]
generateChanges xs = zipWith (-) (drop 1 xs) xs

parseContent :: String -> [Int]
parseContent = map read . lines
