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
monomial :: Dataset Double -> Interaction -> Column Double
monomial xss ks
  | V.length xss == 0 = LA.fromList []
  | otherwise         = M.foldrWithKey monoProduct 1 ks
  where
    --vzero               = LA.fromList $ replicate n 1
    --n                   = LA.size (xss V.! 0)
    monoProduct ix k ps = ps * LA.cmap (^^k) (xss V.! ix)

-- | to evaluate a term we apply the transformation function
-- to the result of 'itTimes'.
evalTerm :: Term -> Dataset Double -> Column Double
evalTerm (Term t ks) xss = LA.cmap t' (monomial xss ks)
  where t' = transform t

-- | evaluates the expression into a list of terms
-- in case the evaluated values are needed
evalExprToList :: Expr -> Dataset Double -> [Column Double]
evalExprToList terms xs = map (`evalTerm` xs) terms

-- | evaluates an expression by evaluating the terms into a list
-- applying the weight and summing the results.
evalExpr :: Expr -> Dataset Double -> [Double] -> Column Double
evalExpr terms xss ws = sum weightedTerms
  where
    weightedTerms = zipWith multWeight ws (evalExprToList terms xss)
    multWeight w  = LA.cmap (w*)

-- w1 t1(p1(x)) + w2 t2(p2(x))
-- w1 t1'(p1(x))p1'(x) + w2 t2'(p2(x))p2'(x)
evalTermDiff :: Term -> Dataset Double -> Int -> Column Double 
evalTermDiff (Term t ks) xss ix 
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

evalDiff :: Expr -> Dataset Double -> [Double] -> Int -> Column Double
evalDiff terms xss ws ix = sum weightedTerms
  where
    weightedTerms = zipWith multWeight ws (map (\t -> evalTermDiff t xss ix) terms)
    multWeight w  = LA.cmap (w*)

evalImage :: Expr -> [Interval Double] -> [Double] -> Interval Double
evalImage terms domains ws = sum weightedTerms
  where
    weightedTerms = zipWith (*) (map singleton ws) (map (evalTermInterval domains) terms)

evalTermInterval :: [Interval Double] -> Term -> Interval Double
evalTermInterval domains (Term t ks) = transform t (monomialInterval domains ks)

monomialInterval :: [Interval Double] -> Interaction -> Interval Double 
monomialInterval domains ks = foldr monoProduct (singleton 1) $ zip [0..] domains 
  where
    monoProduct (ix, x) img 
      | ix `M.member` ks = img * (x ** get ix)
      | otherwise        = img
    get ix = fromIntegral (ks M.! ix)

evalDiffImage :: Expr -> [Interval Double] -> [Double] -> Int -> Interval Double
evalDiffImage terms domains ws ix = sum weightedTerms
  where
    weightedTerms = zipWith (*) (map singleton ws) (map (evalTermDiffInterval domains ix) terms)

evalTermDiffInterval :: [Interval Double] -> Int -> Term -> Interval Double 
evalTermDiffInterval domains ix (Term t ks)
  | M.member ix ks = singleton 0
  | otherwise      = it * p'
  where
    p  = monomialInterval domains ks
    p' = monomialInterval domains (M.update dec ix ks)
    t' = derivative t
    it = t' p

    dec 1 = Nothing
    dec k = Just (k-1)

-- | A value is invalid if it's wether NaN or Infinite
isInvalid :: Double -> Bool
isInvalid x = isNaN x || isInfinite x || abs x >= 1e150

-- | a set of points is valid if none of its values are invalid and
-- the maximum abosolute value is below 1e150 (to avoid overflow)
isValid :: [Double] -> Bool
isValid xs = not (any isInvalid xs)

{- TO BE TESTED
 -- && var > 1e-4
  where
    mu  = sum(xs) / n
    var = (*(1/n)) . sum $ map (\x -> (x-mu)*(x-mu)) xs
    n   = fromIntegral (length xs)
-}

-- | evaluate an expression to a set of samples 
--
-- (1 LA.|||) adds a bias dimension
exprToMatrix :: Dataset Double -> Expr -> LA.Matrix Double
exprToMatrix xss = intercept . LA.fromColumns . map (`evalTerm` xss)
  where
    intercept = (1 LA.|||)

-- | Clean the expression by removing the invalid teerms
cleanExpr :: Dataset Double -> Expr -> Expr
cleanExpr xss [] = []
cleanExpr xss (term:terms) = if ((not . null) . LA.find isInvalid . evalTerm term)  xss
                                then cleanExpr xss terms
                                else term : cleanExpr xss terms 

-- | Checks if the fitness of a solution is not Inf nor NaN.
notInfNan :: Solution -> Bool
notInfNan s = not (isInfinite f || isNaN f)
  where f = head $ _fit s
