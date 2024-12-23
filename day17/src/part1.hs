import Data.Bits (xor)
import Data.Char (isSpace)
import Data.List (intercalate)

data State = State
    { regA :: Int
    , regB :: Int
    , regC :: Int
    , pc   :: Int    -- Program counter
    , prog :: [Int]  -- Program memory
    , outp :: [Int]  -- Collected output
    } deriving Show

fetch :: State -> Maybe (Int, Int)
fetch s =
    let p = pc s
        memory = prog s
    in if p+1 < length memory
       then Just (memory !! p, memory !! (p+1))
       else Nothing

getCombo :: State -> Int -> Int
getCombo s x
    | x <= 3 = x       -- literal 0-3
    | x == 4 = regA s
    | x == 5 = regB s
    | x == 6 = regC s
    | otherwise = error "Invalid combo operand 7 encountered."

isComboOperand :: Int -> Bool
isComboOperand opcode = opcode `elem` [0,2,5,6,7]

divisor :: State -> Int -> Int
divisor s op = 2 ^ getCombo s op

step :: State -> State
step s = case fetch s of
    Nothing -> s -- No more instructions, halt
    Just (opcode, operand) ->
        let nextPC = pc s + 2
            comboVal = if isComboOperand opcode then getCombo s operand else operand
        in case opcode of
            0 -> -- adv
                 s { regA = regA s `div` divisor s operand
                   , pc   = nextPC
                   }
            1 -> -- bxl
                 s { regB = regB s `xor` operand
                   , pc   = nextPC
                   }
            2 -> -- bst
                 s { regB = comboVal `mod` 8
                   , pc   = nextPC
                   }
            3 -> -- jnz
                 if regA s /= 0
                 then s { pc = operand } -- jump
                 else s { pc = nextPC }
            4 -> -- bxc
                 s { regB = regB s `xor` regC s
                   , pc   = nextPC
                   }
            5 -> -- out
                 s { outp = outp s ++ [comboVal `mod` 8]
                   , pc   = nextPC
                   }
            6 -> -- bdv
                 s { regB = regA s `div` divisor s operand
                   , pc   = nextPC
                   }
            7 -> -- cdv
                 s { regC = regA s `div` divisor s operand
                   , pc   = nextPC
                   }
            _ -> error "Invalid opcode encountered."

run :: State -> State
run s = case fetch s of
    Nothing -> s
    Just _  -> run (step s)

parseInput :: FilePath -> IO (Int, Int, Int, [Int])
parseInput filePath = do
    content <- readFile filePath
    let ls         = lines content
        aLine      = ls !! 0
        bLine      = ls !! 1
        cLine      = ls !! 2
        pLine      = ls !! 4
        aVal       = parseRegister aLine "Register A:"
        bVal       = parseRegister bLine "Register B:"
        cVal       = parseRegister cLine "Register C:"
        progList   = parseProgram pLine "Program:"
    return (aVal, bVal, cVal, progList)
  where
    parseRegister :: String -> String -> Int
    parseRegister line prefix =
        let valStr        = drop (length prefix) line
            valStrTrimmed = trim valStr
        in read valStrTrimmed

    parseProgram :: String -> String -> [Int]
    parseProgram line prefix =
        let valsStr        = drop (length prefix) line
            valsStrTrimmed = trim valsStr
            vals           = map (read . trim) (splitBy ',' valsStrTrimmed)
        in vals

    splitBy :: Char -> String -> [String]
    splitBy _ "" = []
    splitBy delim s =
        let (w, rest) = break (== delim) s
        in w : case rest of
                 []     -> []
                 (_:xs) -> splitBy delim xs

    trim :: String -> String
    trim = f . f
      where f = reverse . dropWhile isSpace

main :: IO ()
main = do
    (aVal, bVal, cVal, progList) <- parseInput "input.txt"
    let initialState = State
            { regA = aVal
            , regB = bVal
            , regC = cVal
            , pc   = 0
            , prog = progList
            , outp = []
            }
    let finalState = run initialState
    putStrLn $ intercalate "," (map show (outp finalState))