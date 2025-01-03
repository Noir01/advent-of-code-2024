module Main where

import Data.Array.Unboxed (UArray, listArray, assocs, (!?), (//))
import Data.Ix (Ix(..))
import qualified Data.List.NonEmpty as NE -- (NonEmpty(..), head)
import Data.List (nub)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let grid = parseInput input
    let start = NE.head (NE.fromList [p | (p, '^') <- assocs grid])
    let path1 = nub (map fst (walk grid start north))
    let check2 p = isLoop (walk (grid // [(p, '#')]) start north)

    print $ countBy check2 (drop 1 path1) 

countBy :: Foldable f => (a -> Bool) -> f a -> Int
countBy p = foldl (\acc x -> if p x then acc+1 else acc) 0

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
    (+) (C y1 x1) (C y2 x2)= C (y1 + y2) (x1 + x2)

north :: Coord
north = C (-1) 0

turnRight :: Coord -> Coord
turnRight (C y x) = C x (-y)

walk :: UArray Coord Char -- grid
        -> Coord -- start
        -> Coord -- dir
        -> [(Coord, Coord)] -- [(coord, dir)]
walk grid pos dir =
    (pos, dir) :
        case grid !? (dir + pos) of
            Nothing     ->    []
            Just '#'    ->    walk grid pos (turnRight dir)
            _           ->    walk grid (pos + dir) dir

isLoop :: Eq a => [a] -> Bool
isLoop a = go a a
    where
        go (x:xs) (_:y:ys)  = x == y || go xs ys
        go _ _              = False

parseInput :: String -> UArray Coord Char
parseInput input = listArray (C 0 0, C (length xs - 1) (w - 1)) (concat xs)
    where
        xs = lines input
        w = length (xs !! 1)
