module Main where

import Data.Ix (Ix(..))
import Data.Array.Unboxed (UArray, listArray, elems, assocs, bounds)
import Data.Set (toList, fromList)
import Data.List (tails)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let grid = parseInput input

    let antinodes = length $ toList $ fromList $ concat $ concatMap (map (uncurry (antinodePositions $ bounds grid)) . pairs . findPositions grid) $ uniqueAntennaFrequencies grid

    print antinodes

data Coord = C !Int !Int
    deriving (Show, Ord, Eq)

instance Ix Coord where
    range (C r1 c1, C r2 c2) = 
        [ C r c | r <- [r1..r2], c <- [c1..c2] ]

    inRange (C r1 c1, C r2 c2) (C r c) =
        r1 <= r && r <= r2 && c1 <= c && c <= c2

    index (C r1 c1, C _ c2) (C r c) =
        (r - r1) * (c2 - c1 + 1) + (c - c1)

instance Num Coord where
    (+) (C y1 x1) (C y2 x2) = C (y1 + y2) (x1 + x2)

    (*) (C y1 x1) (C y2 x2) = C (y1 * y2) (x1 * x2)

    negate (C y x) = C (-y) (-x)

    abs (C y x) = C (abs y) (abs x)

    fromInteger n = C (fromInteger n) (fromInteger n)

uniqueAntennaFrequencies :: UArray Coord Char -> [Char]
uniqueAntennaFrequencies grid = toList $ fromList $ filter (/= '.') $ elems grid

findPositions :: UArray Coord Char -> Char -> [Coord]
findPositions grid char = [i | (i, x) <- assocs grid, x == char]

antinodePositions :: (Coord, Coord) -> Coord -> Coord -> [Coord]
antinodePositions (lowerBound, upperBound) p1 p2 =
    let
        C dy dx  = p2 - p1
        gcdVal   = gcd (abs dy) (abs dx)

        step     = C (dy `quot` gcdVal) (dx `quot` gcdVal)

        inRangeGrid = inRange (lowerBound, upperBound)

        forward  = takeWhile inRangeGrid (iterate (+ step) p1)
        backward = takeWhile inRangeGrid (iterate (subtract step) (p1 - step))
    in
        forward ++ backward

pairs :: [Coord] -> [(Coord, Coord)]
pairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

parseInput :: String -> UArray Coord Char
parseInput input = listArray (C 0 0, C (length xs - 1) (w - 1)) (concat xs)
    where
        xs = lines input
        w = length (xs !! 1)
