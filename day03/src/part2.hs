import Text.Regex.TDFA ((=~))
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)
import Data.List (stripPrefix)

-- Main function: Reads input, processes patterns, and computes the result
main :: IO ()
main = do
  content <- readFile "input.txt"
  let pattern = "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)"  -- Match "mul(X,Y)", "do()", and "don't()"
      matches = getAllMatches pattern content                  -- Extract matches from content
      result = processMatches matches
  print result

-- Extract all matching patterns from a string
getAllMatches :: String -> String -> [String]
getAllMatches pattern input =
  let matches = input =~ pattern :: [[String]]
  in mapMaybe listToMaybe matches

-- Helper function: Extract the first match from a list
listToMaybe :: [String] -> Maybe String
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

-- Process the list of matches and compute the result
processMatches :: [String] -> Int
processMatches = go True 0
  where
    go :: Bool -> Int -> [String] -> Int
    go _ result [] = result  -- Base case: return the accumulated result
    go flag result (match:rest)
      | match == "do()"      = go True result rest    -- Turn flag ON
      | match == "don't()"   = go False result rest   -- Turn flag OFF
      | take 3 match == "mul" && flag =
          case parseMul match of
            Just value -> go flag (result + value) rest
            Nothing    -> go flag result rest
      | otherwise = go flag result rest  -- Skip unmatched patterns

-- Parse "mul(X,Y)" and compute the product of X and Y
parseMul :: String -> Maybe Int
parseMul str =
  case stripPrefix "mul(" str of
    Just rest ->
      let withoutParen = takeWhile (/= ')') rest
          nums = splitBy ',' withoutParen
      in case mapM readMaybe nums of
           Just [x, y] -> Just (x * y)
           _           -> Nothing
    Nothing -> Nothing

-- Split a string by a delimiter
splitBy :: Char -> String -> [String]
splitBy delim = foldr f [[]]
  where
    f c acc@(cur:rest)
      | c == delim = [] : acc
      | otherwise  = (c : cur) : rest

-- Safe read function to parse numbers
readMaybe :: String -> Maybe Int
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing