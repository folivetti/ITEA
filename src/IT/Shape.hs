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

data Shape   = Image (Double, Double) 
             | DiffImg Int (Double, Double) | NonIncreasing Int | NonDecreasing Int
             | PartialNonIncreasing Int (Double, Double) | PartialNonDecreasing Int (Double, Double)
             | Inflection Int Int | Convex Int Int | Concave Int Int -- (0,0), (0,Infinity), (-Infinitiy,0)
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
        loDiff     = if lo' > lo && lo' < hi then 0 else abs (lo - lo')
        hiDiff     = if hi' < hi && hi' > lo then 0 else abs (hi' - hi)
    in  loDiff + hiDiff
  where 
    img' = imgfun domains expr ws

violationNonIncreasing, violationNonDecreasing :: Int -> [Interval Double] -> Constraint
violationNonIncreasing ix domains expr ws  -- (-Infinity .. 0)
  | img == empty = 1e+10
  | otherwise    = max 0 $ sup img
  where img = evalDiffImage ix domains expr ws

violationNonDecreasing ix domains expr ws -- (0 .. Infinity)
  | img == empty = 1e+10
  | otherwise    = negate $ min 0 $ inf img
  where img = evalDiffImage ix domains expr ws

violationInflection, violationConcave, violationConvex :: Int -> Int -> [Interval Double] -> Constraint
violationInflection ix iy domains expr ws -- (0 .. 0)
  | img == empty = 1e+10
  | otherwise    = if abs (inf img) + abs (sup img) < 1e-2 then 0 else abs (inf img) + abs (sup img)
  where img = evalSndDiffImage ix iy domains expr ws

violationConvex ix iy domains expr ws -- (0 .. Infinity)
  | img == empty = 1e+10
  | otherwise    = negate $ min 0 $ inf img
  where img = evalSndDiffImage ix iy domains expr ws

violationConcave ix iy domains expr ws -- (-Infinity .. 0)
  | img == empty = 1e+10
  | otherwise    = max 0 $ sup img
  where img = evalSndDiffImage ix iy domains expr ws

constraintFrom :: [Constraint] -> Constraint
constraintFrom funs expr ws = let c = foldr (\f tot -> abs (f expr ws) + tot) 0 funs
                              in if c < 1e-60 then 0 else c

fromShapes :: [Shape] -> Domains -> Constraint
fromShapes _      Nothing        = unconstrained
fromShapes []     _              = unconstrained
fromShapes shapes (Just domains) = constraintFrom (map toFun shapes)
  where
    domains' = map (uncurry (...)) domains
    replace ds ix rng = let rng' = (fst rng ... snd rng)
                        in  take ix ds ++ (rng' : drop (ix+1) ds)

    toFun (Image (lo, hi))      = violationImg evalImage          domains' (lo ... hi) 
    toFun (DiffImg ix (lo, hi)) = violationImg (evalDiffImage ix) domains' (lo ... hi) 
    toFun (NonIncreasing ix)    = violationNonIncreasing ix domains' 
    toFun (NonDecreasing ix)    = violationNonDecreasing ix domains' 
    toFun (PartialNonIncreasing ix range) = violationNonIncreasing ix $ replace domains' ix range
    toFun (PartialNonDecreasing ix range) = violationNonDecreasing ix $ replace domains' ix range
    toFun (Inflection ix iy)    = violationInflection ix iy domains'
    toFun (Convex ix iy)        = violationConvex ix iy domains'
    toFun (Concave ix iy)       = violationConcave ix iy domains'

