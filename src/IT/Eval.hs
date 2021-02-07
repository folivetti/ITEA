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

import Foreign.Storable
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Map.Strict as M
import Numeric.Interval hiding (null)

import IT
import IT.Algorithms

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

derivative :: Floating a => Transformation -> a -> a
derivative Id      = const 1
derivative Sin     = cos
derivative Cos     = negate.sin
derivative Tan     = recip . (^2) . cos
derivative Tanh    = (1-) . (^2) . tanh
derivative Sqrt    = recip . (2*) . sqrt
derivative SqrtAbs = \x -> x / (2* (abs x)**1.5)
derivative Exp     = exp
derivative Log     = recip

-- Interaction of dataset xss with strengths ks
-- ps = xss ** ks
-- it is faster to go through the nonzero strengths, since we can query a column in O(1)
monomial :: Dataset Double -> Interaction -> Column Double
monomial xss ks
  | V.length xss == 0 = LA.fromList []
  | otherwise         = M.foldrWithKey monoProduct 1 ks
  where
    --vzero               = LA.fromList $ replicate n 1
    --n                   = LA.size (xss V.! 0)
    monoProduct ix k ps = ps * LA.cmap (^^k) (xss V.! ix)

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
evalTerm xss (Term t ks) = LA.cmap t' (monomial xss ks)
  where t' = transform t

-- | apply the transformation function to the interaction monomial
evalTermInterval :: [Interval Double] -> Term -> Interval Double
evalTermInterval domains (Term t ks) = transform t (monomialInterval domains ks)

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
  | M.member ix ks = LA.fromList $ replicate n 0
  | otherwise      = it * p'
  where
    p  = monomial xss ks
    p' = monomial xss (M.update dec ix ks)
    t' = derivative t
    it = LA.cmap t' p
    n  = LA.size (xss V.! 0)

    dec 1 = Nothing
    dec k = Just (k-1)
    
evalTermDiffInterval :: Int -> [Interval Double] -> Term -> Interval Double 
evalTermDiffInterval ix domains (Term t ks)
  | M.member ix ks = singleton 0
  | otherwise      = it * p'
  where
    p  = monomialInterval domains ks
    p' = monomialInterval domains (M.update dec ix ks)
    t' = derivative t
    it = t' p

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


-- | A value is invalid if it's wether NaN or Infinite
isInvalid :: Double -> Bool
isInvalid x = isNaN x || isInfinite x || abs x >= 1e150

-- | a set of points is valid if none of its values are invalid and
-- the maximum abosolute value is below 1e150 (to avoid overflow)
isValid :: [Double] -> Bool
isValid xs = not (any isInvalid xs)

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
