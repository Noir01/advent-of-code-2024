module LinearMin (minimize3aPlusB) where

import Data.Ratio (Ratio, (%), denominator, numerator)

-- | Given two linear equations with integer coefficients:
--      a*x1 + b*y1 = c1
--      a*x2 + b*y2 = c2
--   This function returns Just (3a+b) if a "minimum" can be determined, or Nothing otherwise.
--   All inputs and outputs are integers. No floating point arithmetic is used.
minimize3aPlusB :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Maybe Integer
minimize3aPlusB x1 y1 c1 x2 y2 c2 =
  let det = x1*y2 - x2*y1
  in if det /= 0
     then -- Unique solution
          -- a = (c1*y2 - c2*y1) / det
          -- b = (x1*c2 - x2*c1) / det
          let aNum = (c1*y2 - c2*y1) % det
              bNum = (x1*c2 - x2*c1) % det
              val  = (3 % 1)*aNum + bNum
          in rationalToMaybeInteger val
     else
       -- det == 0 -> either no solution or infinitely many solutions
       let consistent =
             -- Check if (x1,y1,c1) and (x2,y2,c2) are proportional
             -- Avoid dividing by zero by checking patterns:
             if x1 == 0 && y1 == 0 && c1 == 0
             then (x2 == 0 && y2 == 0 && c2 == 0)  -- all zero => infinite solutions
             else if x1 /= 0
                  then let kNumX = (x2 % x1)
                           checkY = (y2 % 1) == kNumX*(y1 % 1)
                           checkC = (c2 % 1) == kNumX*(c1 % 1)
                       in checkY && checkC
                  else if y1 /= 0
                       then let kNumY = (y2 % y1)
                                checkX = (x2 % 1) == kNumY*(x1 % 1)
                                checkC = (c2 % 1) == kNumY*(c1 % 1)
                            in checkX && checkC
                       else if c1 /= 0
                            then let kNumC = (c2 % c1)
                                     checkX = (x2 % 1) == kNumC*(x1 % 1)
                                     checkY = (y2 % 1) == kNumC*(y1 % 1)
                                 in checkX && checkY
                            else False
       in if not consistent
          then Nothing  -- no solution
          else
            -- Infinitely many solutions.
            -- We'll use the first equation: a*x1 + b*y1 = c1
            -- If y1 == 0:
            --   - If x1 == 0, then c1 == 0 as well (since consistent). 
            --     All (a,b) are solutions, no minimum for 3a+b.
            --   - Else a = c1/x1 (exact?), b is free, so 3a+b is unbounded.
            -- If y1 /= 0:
            --   Solve for b: b = (c1 - a*x1)/y1
            --   3a+b = 3a + (c1/y1) - (x1/y1)*a = (3 - (x1/y1))*a + c1/y1
            -- If slope = (3 - (x1/y1)) = 0 => 3a+b = constant = c1/y1
            -- else no minimum.
            if y1 == 0
            then
              if x1 == 0
              then
                -- 0 = c1 => c1 must be 0. All solutions possible, 3a+b unbounded
                Nothing
              else
                -- a = c1/x1, b free. 
                -- 3a+b = 3*(c1/x1)+b is unbounded since b can vary infinitely.
                Nothing
            else
              let slope     = (3 % 1) - (x1 % y1)
                  intercept = (c1 % y1)
              in if slope == 0
                 then
                   -- 3a+b is constant = intercept
                   rationalToMaybeInteger intercept
                 else
                   -- slope /= 0 => can vary without bound
                   Nothing

-- | Convert a Rational to Maybe Integer, returning Just if the Rational is actually an integer.
rationalToMaybeInteger :: Rational -> Maybe Integer
rationalToMaybeInteger r =
  if denominator r == 1
  then Just (numerator r)
  else Nothing
