import qualified Data.Vector as V

main :: IO ()
main = do
  content <- readFile "input.txt"

  let charArray = parseTo2DVector content

  let possibleCombinations = concatMap possibleValidDirections $ findAllXs charArray

  print $ length $ filter id $ map (isXMAS charArray) possibleCombinations

isXMAS :: V.Vector (V.Vector Char) -> [(Int, Int)] -> Bool
isXMAS _ [_] = False
isXMAS _ [_, _] = False
isXMAS _ [_, _, _] = False
isXMAS charArray [_, m, a, s] =
  ((charArray V.! fst m) V.! snd m) == 'M' && ((charArray V.! fst a) V.! snd a) == 'A' && ((charArray V.! fst s) V.! snd s) == 'S'
isXMAS _ _ = False

findAllXs :: V.Vector (V.Vector Char) -> [(Int, Int)]
findAllXs = searchRows 0
  where
    searchRows :: Int -> V.Vector (V.Vector Char) -> [(Int, Int)]
    searchRows _ rows | V.null rows = []
    searchRows rowIndex rows =
      let currentRow = V.head rows
          xsInRow = searchCols rowIndex 0 currentRow
      in xsInRow ++ searchRows (rowIndex + 1) (V.tail rows)

    searchCols :: Int -> Int -> V.Vector Char -> [(Int, Int)]
    searchCols _ _ cols | V.null cols = []
    searchCols rowIndex colIndex cols =
      let currentChar = V.head cols
          restResult = searchCols rowIndex (colIndex + 1) (V.tail cols)
      in if currentChar == 'X'
           then (rowIndex, colIndex) : restResult
           else restResult

possibleValidDirections :: (Int, Int) -> [[(Int, Int)]]
possibleValidDirections (x, y) = filter (all validCoordinate) directions
  where
    directions =
      [
        [(x, y), (x, y - 1), (x, y - 2), (x, y - 3)],
        [(x, y), (x, y + 1), (x, y + 2), (x, y + 3)],
        [(x, y), (x - 1, y), (x - 2, y), (x - 3, y)],
        [(x, y), (x + 1, y), (x + 2, y), (x + 3, y)],
        [(x, y), (x - 1, y - 1), (x - 2, y - 2), (x - 3, y - 3)],
        [(x, y), (x + 1, y + 1), (x + 2, y + 2), (x + 3, y + 3)],
        [(x, y), (x + 1, y - 1), (x + 2, y - 2), (x + 3, y - 3)],
        [(x, y), (x - 1, y + 1), (x - 2, y + 2), (x - 3, y + 3)]
      ]

validCoordinate :: (Int, Int) -> Bool
validCoordinate (x, y) =
  0 <= x && x < 140 && 0 <= y && y < 140

parseTo2DVector :: String -> V.Vector (V.Vector Char)
parseTo2DVector content =
  V.fromList $ map V.fromList (lines content)