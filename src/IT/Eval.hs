{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : IT.Regression
Description : Specific functions for Symbolic Regression
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Definitions of IT data structure and support functions.
-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module IT.Eval where

import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import qualified Data.IntMap.Strict as M
import Numeric.Interval hiding (null)

import IT
import IT.Algorithms

log1p :: Floating a => a -> a
log1p x = log (x+1)

-- * Transformation evaluation
transform :: Floating a => Transformation -> a -> a
transform Id      = id
transform Sin     = sin
transform Cos     = cos
transform Tan     = tan
transform Tanh    = tanh
transform Sqrt    = sqrt
transform SqrtAbs = sqrt.abs
transform Exp     = exp
transform Log     = log
transform Log1p   = log1p

derivative :: Floating a => Transformation -> a -> a
derivative Id      = const 1
derivative Sin     = cos
derivative Cos     = negate.sin
derivative Tan     = recip . (**2.0) . cos
derivative Tanh    = (1-) . (**2.0) . tanh
derivative Sqrt    = recip . (2*) . sqrt
derivative SqrtAbs = \x -> x / (2* abs x**1.5)
derivative Exp     = exp
derivative Log     = recip
derivative Log1p   = recip . (+1)

sndDerivative :: Floating a => Transformation -> a -> a
sndDerivative Id      = const 0
sndDerivative Sin     = negate.sin
sndDerivative Cos     = negate.cos
sndDerivative Tan     = \x -> 2 * tan x * cos x ** (-2.0)
sndDerivative Tanh    = \x -> (-2) * tanh x * (1 - tanh x ** 2.0)
sndDerivative Sqrt    = negate . recip . (*4) . sqrt . (**3.0)
sndDerivative SqrtAbs = \x -> -(x**2.0 / (4 * abs x ** 3.5)) -- assuming dirac(x) = 0
sndDerivative Exp     = exp
sndDerivative Log     = \x -> -(1/x**2)
sndDerivative Log1p   = negate . recip . (**2.0) . (+1)

-- Interaction of dataset xss with strengths ks
-- ps = xss ** ks
-- it is faster to go through the nonzero strengths, since we can query a column in O(1)
monomial :: Dataset Double -> Interaction -> Column Double
monomial xss ks
  | V.length xss == 0 = LA.fromList []
  | otherwise         = M.foldrWithKey monoProduct 1 ks

  where
    monoProduct ix k ps = ps * (xss V.! ix) ^^ k

-- | Interaction of the domain interval
-- it is faster to fold through the list of domains and checking whether we 
-- have a nonzero strength.
monomialInterval :: [Interval Double] -> Interaction -> Interval Double
monomialInterval domains ks = foldr monoProduct (singleton 1) $ zip [0..] domains
  where
    monoProduct (ix, x) img
      | ix `M.member` ks = img * (x ** get ix)
      | otherwise        = img
    get ix = fromIntegral (ks M.! ix)

-- | to evaluate a term we apply the transformation function
-- to the interaction monomial
evalTerm :: Dataset Double -> Term -> Column Double
evalTerm xss (Term t ks) = transform t (monomial xss ks)

-- | apply the transformation function to the interaction monomial
evalTermInterval :: [Interval Double] -> Term -> Interval Double
evalTermInterval domains (Term t ks)
  | inter == empty = empty
  | otherwise      = protected (transform t) inter
  where inter = monomialInterval domains ks

-- | The partial derivative of a term w.r.t. the variable ix is:
-- 
-- If ix exists in the interaction, the derivative is:
--      derivative t (monomial xss ks) * (monomial xss ks') 
--      ks' is the same as ks but with the strength of ix decremented by one
-- Otherwise it is equal to 0
--
-- w1 t1(p1(x)) + w2 t2(p2(x))
-- w1 t1'(p1(x))p1'(x) + w2 t2'(p2(x))p2'(x)
evalTermDiff :: Int -> Dataset Double -> Term -> Column Double
evalTermDiff ix xss (Term t ks)
  | M.member ix ks = ki * it * p'
  | otherwise      = LA.fromList $ replicate n 0
  where
    ki = fromIntegral $ ks M.! ix
    p  = monomial xss ks
    p' = monomial xss (M.update dec ix ks)
    t' = derivative t
    it = LA.cmap t' p
    n  = LA.size (xss V.! 0)

    dec 1 = Nothing
    dec k = Just (k-1)

-- w1 t1''(p1(x))p1'(x)p1'(x) + w1 t1'(p1(x))p1''(x)
evalTermSndDiff :: Int -> Int -> Dataset Double -> Term -> Column Double
evalTermSndDiff ix iy xss (Term t ks)
  | M.member ix ks' && M.member iy ks = ki*kj*tp''*px'*py' + kij*tp'*pxy'
  | otherwise                         = LA.fromList $ replicate n 0
  where
    ks'  = M.update dec iy ks
    ki = fromIntegral $ ks M.! ix
    kj =  fromIntegral $  ks M.! iy
    kij = kj * (fromIntegral (ks' M.! ix))
    p    = monomial xss ks
    px'  = monomial xss (M.update dec ix ks)
    py'  = monomial xss (M.update dec iy ks)
    pxy' = monomial xss (M.update dec ix ks')
    t'   = derivative t
    t''  = sndDerivative t
    tp'  = LA.cmap t'  p
    tp'' = LA.cmap t'' p
    n    = LA.size (xss V.! 0)

    dec 1 = Nothing
    dec k = Just (k-1)

evalTermDiffInterval :: Int -> [Interval Double] -> Term -> Interval Double
evalTermDiffInterval ix domains (Term t ks)
  | M.member ix ks = ki * it * p'
  | otherwise      = singleton 0
  where
    ki = (singleton . fromIntegral) $ ks M.! ix
    p  = monomialInterval domains ks
    p' = monomialInterval domains (M.update dec ix ks)
    t' = protected (derivative t)
    it = t' p

    dec 1 = Nothing
    dec k = Just (k-1)

evalTermSndDiffInterval :: Int -> Int -> [Interval Double] -> Term -> Interval Double
evalTermSndDiffInterval ix iy domains (Term t ks)
  | M.member ix ks' && M.member iy ks = ki*kj*tp''*px'*py' + kij*tp'*pxy'
  | otherwise                         = singleton 0
  where
    ks' = M.update dec iy ks
    ki = (singleton . fromIntegral) $ ks M.! ix
    kj = (singleton . fromIntegral) $ ks M.! iy
    kij = kj * (singleton $ fromIntegral (ks' M.! ix))
    p    = monomialInterval domains ks
    px'  = monomialInterval domains (M.update dec ix ks)
    py'  = monomialInterval domains (M.update dec iy ks)
    pxy' = monomialInterval domains (M.update dec ix $ M.update dec iy ks)
    t'   = protected (derivative t)
    t''  = protected (sndDerivative t)
    tp'  = t'  p
    tp'' = t'' p

    dec 1 = Nothing
    dec k = Just (k-1)

-- | evaluates an expression by evaluating the terms into a list
-- applying the weight and summing the results.
evalGeneric :: (Dataset Double -> Term -> Column Double) -> Dataset Double -> Expr -> [Double] -> Column Double
evalGeneric f xss terms ws = sum weightedTerms
  where
    weightedTerms = zipWith multWeight ws (map (f xss) terms)
    multWeight w  = LA.cmap (w*)

evalExpr :: Dataset Double -> Expr -> [Double] -> Column Double
evalExpr = evalGeneric evalTerm

evalDiff :: Int -> Dataset Double -> Expr -> [Double] -> Column Double
evalDiff ix = evalGeneric (evalTermDiff ix)

evalSndDiff :: Int -> Int -> Dataset Double -> Expr -> [Double] -> Column Double
evalSndDiff ix iy = evalGeneric (evalTermSndDiff ix iy)

-- | Returns the estimate of the image of the funcion with Interval Arithmetic
--
-- this requires a different implementation from `evalGeneric`
evalImageGeneric :: ([Interval Double] -> Term -> Interval Double) -> [Interval Double]-> Expr -> [Double] -> Interval Double
evalImageGeneric f domains terms ws = sum weightedTerms
  where
    weightedTerms = zipWith (*) (map singleton ws) (map (f domains) terms)

evalImage :: [Interval Double] -> Expr -> [Double] -> Interval Double
evalImage = evalImageGeneric evalTermInterval

evalDiffImage :: Int -> [Interval Double] -> Expr -> [Double] -> Interval Double
evalDiffImage ix = evalImageGeneric (evalTermDiffInterval ix)

evalSndDiffImage :: Int -> Int -> [Interval Double] -> Expr -> [Double] -> Interval Double
evalSndDiffImage ix iy = evalImageGeneric (evalTermSndDiffInterval ix iy)

-- | A value is invalid if it's wether NaN or Infinite
isInvalid :: Double -> Bool
isInvalid x = isNaN x || isInfinite x || abs x >= 1e150

-- | a set of points is valid if none of its values are invalid and
-- the maximum abosolute value is below 1e150 (to avoid overflow)
isValid :: [Double] -> Bool
isValid = all (\x -> not (isNaN x) && not (isInfinite x) && abs x < 1e150) 
-- not (any isInvalid xs)

-- | evaluate an expression to a set of samples 
--
-- (1 LA.|||) adds a bias dimension
exprToMatrix :: Dataset Double -> Expr -> LA.Matrix Double
exprToMatrix xss = intercept . LA.fromColumns . map (evalTerm xss)
  where
    intercept = (1 LA.|||)

-- | Clean the expression by removing the invalid teerms
cleanExpr :: Dataset Double -> Expr -> Expr
cleanExpr xss [] = []
cleanExpr xss (term:terms) = if not . null $ LA.find isInvalid $ evalTerm xss term
                                then cleanExpr xss terms
                                else term : cleanExpr xss terms

-- | Checks if the fitness of a solution is not Inf nor NaN.
notInfNan :: Solution -> Bool
notInfNan s = not (isInfinite f || isNaN f)
  where f = head $ _fit s

-- | definition of an interval evaluated to NaN
nanInterval :: RealFloat a => Interval a
nanInterval = singleton (0/0)

-- | Check if any bound of the interval is infinity
hasInf :: RealFloat a => Interval a -> Bool
hasInf x = any isInfinite [inf x, sup x]

-- | Creates a protected function to avoid throwing exceptions
protected :: RealFloat a => (Interval a -> Interval a) -> Interval a -> Interval a
protected f x
  | hasInf x      = nanInterval
  | sup y < inf y = sup y ... inf y
  | otherwise     = y
  where
    y = f x
