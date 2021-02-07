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

-- * Constraint functions
unconstrained :: Constraint
unconstrained _ _ = 0

type ImgFun = [Interval Double] -> Expr -> [Double] -> Interval Double

-- violationImg evalImage
-- violationImg (evalDiffImage ix)
violationImg :: ImgFun -> [Interval Double] -> Interval Double -> Constraint
violationImg imgfun domains img expr ws = 
  let (lo, hi)   = (inf &&& sup) img
      (lo', hi') = (inf &&& sup) $ imgfun domains expr ws
      loDiff     = if lo' > lo then 0 else lo - lo'
      hiDiff     = if hi' < hi then 0 else hi' - hi
  in  loDiff + hiDiff

violationNonIncreasing, violationNonDecreasing :: Int -> [Interval Double] -> Constraint
violationNonIncreasing ix domains expr ws = min 0 $ sup $ evalDiffImage ix domains expr ws
violationNonDecreasing ix domains expr ws = abs $ max 0 $ inf $ evalDiffImage ix domains expr ws


constraintFrom :: [Constraint] -> Constraint
constraintFrom funs expr ws = map (\f -> f expr ws) funs
