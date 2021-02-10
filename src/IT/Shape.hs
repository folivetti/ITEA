{-|
Module      : IT.Shape
Description : Measuring shape constraints with interval arithmetic
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Support functions to measure imposed shape constraints into the image of the function and its derivatives.
-}

module IT.Shape where

import IT
import IT.Eval
import IT.Algorithms

import Control.Arrow
import Numeric.Interval

type ImgFun = [Interval Double] -> Expr -> [Double] -> Interval Double

data Shape   = Image (Double, Double) | DiffImg Int (Double, Double) | NonIncreasing Int | NonDecreasing Int
                    deriving (Show, Read)

type Domains = Maybe [(Double, Double)]

-- * Constraint functions
unconstrained :: Constraint
unconstrained _ _ = 0
                    
-- violationImg evalImage
-- violationImg (evalDiffImage ix)
violationImg :: ImgFun -> [Interval Double] -> Interval Double -> Constraint
violationImg imgfun domains img expr ws 
  | img' == empty = 1e+10
  | otherwise =
    let (lo, hi)   = (inf &&& sup) img
        (lo', hi') = (inf &&& sup) img' 
        loDiff     = if lo' > lo then 0 else lo - lo'
        hiDiff     = if hi' < hi then 0 else hi' - hi
    in  loDiff + hiDiff
  where 
    img' = imgfun domains expr ws

violationNonIncreasing, violationNonDecreasing :: Int -> [Interval Double] -> Constraint
violationNonIncreasing ix domains expr ws
  | img == empty = 1e+10
  | otherwise    = min 0 $ sup img
  where img = evalDiffImage ix domains expr ws

violationNonDecreasing ix domains expr ws 
  | img == empty = 1e+10
  | otherwise    = negate $ max 0 $ inf img
  where img = evalDiffImage ix domains expr ws


constraintFrom :: [Constraint] -> Constraint
constraintFrom funs expr ws = foldr (\f tot -> f expr ws + tot) 0 funs

fromShapes :: [Shape] -> Domains -> Constraint
fromShapes []     Nothing        = unconstrained
fromShapes []     _              = unconstrained
fromShapes shapes (Just domains) = constraintFrom (map toFun shapes)
  where
    domains' = map (uncurry (...)) domains

    toFun (Image (lo, hi))      = violationImg evalImage          domains' (lo ... hi) 
    toFun (DiffImg ix (lo, hi)) = violationImg (evalDiffImage ix) domains' (lo ... hi) 
    toFun (NonIncreasing ix)    = violationNonIncreasing ix domains' 
    toFun (NonDecreasing ix)    = violationNonDecreasing ix domains' 
