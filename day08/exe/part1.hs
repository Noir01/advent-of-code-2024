module Main where

import Data.Ix (Ix(..))
import Data.Array.Unboxed (UArray, listArray, elems, assocs, (!?), bounds)
import Data.Set (toList, fromList)
import Data.List (tails)
import Data.Maybe (isJust, mapMaybe)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let grid = parseInput input

    let antinodes = validCoords grid $ toList $ fromList $ concat (concatMap (map (uncurry antinodePositions) . pairs . findPositions grid) (uniqueAntennaFrequencies grid))

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

    fromInteger n = C (fromInteger n) (fromInteger n)


uniqueAntennaFrequencies :: UArray Coord Char -> [Char]
uniqueAntennaFrequencies grid = toList $ fromList $ filter (/= '.') $ elems grid

findPositions :: UArray Coord Char -> Char -> [Coord]
findPositions grid char = [i | (i, x) <- assocs grid, x == char]

antinodePositions :: Coord -> Coord -> [Coord]
antinodePositions c1 c2 = [2 * c2 - c1, 2 * c1 - c2]

pairs :: [Coord] -> [(Coord, Coord)]
pairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

validCoords :: UArray Coord Char -> [Coord] -> Int
validCoords grid coords = length $ map Just (mapMaybe (grid !?) coords)

parseInput :: String -> UArray Coord Char
parseInput input = listArray (C 0 0, C (length xs - 1) (w - 1)) (concat xs)
    where
        xs = lines input
        w = length (xs !! 1)
