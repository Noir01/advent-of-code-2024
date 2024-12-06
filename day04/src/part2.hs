import qualified Data.Vector as V

main :: IO ()
main = do
  content <- readFile "input.txt"
  let charArray = parseTo2DVector content
      (rows, cols) = gridBounds charArray
      count = length
        [ ()
        | posA <- findAllAs charArray
        , Just positions <- [possibleValidDirections posA rows cols]
        , isXMAS charArray positions
        ]
  print count

parseTo2DVector :: String -> V.Vector (V.Vector Char)
parseTo2DVector = V.fromList . map V.fromList . lines

gridBounds :: V.Vector (V.Vector Char) -> (Int, Int)
gridBounds charArray = (V.length charArray, V.length (V.head charArray))

findAllAs :: V.Vector (V.Vector Char) -> [(Int, Int)]
findAllAs charArray =
  [ (rowIndex, colIndex)
  | (rowIndex, row) <- V.toList $ V.indexed charArray
  , (colIndex, 'A') <- V.toList $ V.indexed row
  ]

possibleValidDirections :: (Int, Int) -> Int -> Int -> Maybe [(Int, Int)]
possibleValidDirections (x, y) rows cols =
  let positions =
        [ (x - 1, y - 1)
        , (x + 1, y - 1)
        , (x, y)
        , (x - 1, y + 1)
        , (x + 1, y + 1)
        ]
      valid (x', y') = 0 <= x' && x' < rows && 0 <= y' && y' < cols
  in if all valid positions then Just positions else Nothing

isXMAS :: V.Vector (V.Vector Char) -> [(Int, Int)] -> Bool
isXMAS charArray positions = case positions of
  [p, q, _, r, s] ->
    let getCharAt (x, y) = (charArray V.! x) V.! y
        c1 = getCharAt p
        c2 = getCharAt s
        c3 = getCharAt q
        c4 = getCharAt r
        matchChars a b = (a == 'M' && b == 'S') || (a == 'S' && b == 'M')
    in matchChars c1 c2 && matchChars c3 c4
  _ -> False