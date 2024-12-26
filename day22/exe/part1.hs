module Main where

import Data.Bits (shiftL, shiftR, xor, (.&.))

main :: IO ()
main = readFile "input.txt" >>= print . part1 . parseContent

part1 :: [Int] -> Int
part1 secretNumbers = sum $ map (applyNTimes nextSecretNumber 2000) secretNumbers

applyNTimes :: (Int -> Int) -> Int -> Int -> Int
applyNTimes f n x = iterate f x !! n

nextSecretNumber :: Int -> Int
nextSecretNumber currentNumber = ((b `shiftL` 11) `xor` b) .&. 0xFFFFFF
    where
        a = ((currentNumber `shiftL` 6) `xor` currentNumber) .&. 0xFFFFFF
        b = ((a `shiftR` 5) `xor` a) .&. 0xFFFFFF

parseContent :: String -> [Int]
parseContent = map read . lines
