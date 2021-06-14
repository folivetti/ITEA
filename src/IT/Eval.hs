{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : IT.Eval
Description : Evaluation function for IT expressions.
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Definition of functions pertaining to the conversion
and evaluation of an IT-expression. 

TODO: move interval evaluation to IT.Eval.Interval 
-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module IT.Eval where

import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import qualified Data.IntMap.Strict as M
import Numeric.Interval hiding (null)
import qualified Numeric.Interval as I
import Data.Bifunctor

import IT
import IT.Algorithms

-- | log(x+1)
log1p :: Floating a => a -> a
log1p x = log (x+1)

-- * Evaluation of the Transformation functions 
-- It supports any type that is an instance of Floating.

-- | Evaluate the transformation function 'f' in a data point 'x'.
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

-- | Evaluate the derivative of a transformation function. 
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

-- | Evaluate the second derivative of the transformation function. 
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

-- | Evaluates the interaction 'ks' in the dataset 'xss'
--
-- It folds the map of exponents with the key, retrieve the corresponding column,
-- calculates x_i^k_i and multiplies to the accumulator.
-- It return a column vector. 
polynomial :: Dataset Double -> Interaction -> Column Double
polynomial xss ks
  | V.length xss == 0 = LA.fromList []
  | otherwise         = M.foldrWithKey monoProduct 1 ks

  where
    monoProduct ix k ps = ps * (xss V.! ix) ^^ k

-- | Evaluates the interval of the interaction w.r.t. the domains of the variables.
--
-- In this case it is faster to fold through the list of domains and checking whether we 
-- have a nonzero strength.
polynomialInterval :: [Interval Double] -> Interaction -> Interval Double
polynomialInterval domains ks = foldr monoProduct (singleton 1) $ zip [0..] domains
  where
    monoProduct (ix, x) img
      | ix `M.member` ks = img * x ^^ get ix
      | otherwise        = img
    get ix = fromIntegral (ks M.! ix)

-- | Evaluates a term by applying the transformation function to
-- the result of the interaction.
evalTerm :: Dataset Double -> Term -> Column Double
evalTerm xss (Term t ks) = transform t (polynomial xss ks)

-- | Evaluates the term on the interval domains. 
evalTermInterval :: [Interval Double] -> Term -> Interval Double
evalTermInterval domains (Term t ks)
  | inter == empty = empty
  | otherwise      = protected (transform t) inter
  where inter = polynomialInterval domains ks

-- | The partial derivative of a term w.r.t. the variable ix is:
-- 
-- If ix exists in the interaction, the derivative is:
--      derivative t (polynomial xss ks) * (polynomial xss ks') 
--      ks' is the same as ks but with the strength of ix decremented by one
-- Otherwise it is equal to 0
--
-- Given the expression: w1 t1(p1(x)) + w2 t2(p2(x))
-- The derivative is: w1 t1'(p1(x))p1'(x) + w2 t2'(p2(x))p2'(x)
evalTermDiff :: Int -> Dataset Double -> Term -> Column Double
evalTermDiff ix xss (Term t ks)
  | M.member ix ks = ki * it * p'
  | otherwise      = LA.fromList $ replicate n 0
  where
    ki = fromIntegral $ ks M.! ix
    p  = polynomial xss ks
    p' = polynomial xss (M.update dec ix ks)
    t' = derivative t
    it = LA.cmap t' p
    n  = LA.size (xss V.! 0)

    dec 1 = Nothing
    dec k = Just (k-1)

-- | The second partial derivative of a term w.r.t. the variables ix and iy is:
-- 
-- given p(x) as the interaction function, t(x) the transformation function,
-- f'(x)|i the first derivative of a function given i and f''(x) the second derivative. 
--
-- If iy exists in p(x) and ix exists in p'(x)|iy, the derivative is:
--
--     kx*ky*t''(x)*p'(x)|ix * p'(x)|iy + kxy*t'(x)p''(x)
--
-- where kx, ky are the original exponents of variables ix and iy,
--     and kxy is ky * kx', with kx' the exponent of ix on p'(x)|iy
--
evalTermSndDiff :: Int -> Int -> Dataset Double -> Term -> Column Double
evalTermSndDiff ix iy xss (Term t ks)
  | M.member ix ks' && M.member iy ks = ki*kj*tp''*px'*py' + kij*tp'*pxy'
  | otherwise                         = LA.fromList $ replicate n 0
  where
    ks'  = M.update dec iy ks
    ki = fromIntegral $ ks M.! ix
    kj =  fromIntegral $  ks M.! iy
    kij = kj * fromIntegral (ks' M.! ix)
    p    = polynomial xss ks
    px'  = polynomial xss (M.update dec ix ks)
    py'  = polynomial xss (M.update dec iy ks)
    pxy' = polynomial xss (M.update dec ix ks')
    t'   = derivative t
    t''  = sndDerivative t
    tp'  = LA.cmap t'  p
    tp'' = LA.cmap t'' p
    n    = LA.size (xss V.! 0)

    dec 1 = Nothing
    dec k = Just (k-1)

-- | Same as 'evalTermDiff' but optimized for intervals. 
evalTermDiffInterval :: Int -> [Interval Double] -> Term -> Interval Double
evalTermDiffInterval ix domains (Term t ks)
  | M.member ix ks = ki * it * p'
  | otherwise      = singleton 0
  where
    ki = singleton . fromIntegral $ ks M.! ix
    p  = polynomialInterval domains ks
    p' = polynomialInterval domains (M.update dec ix ks)
    t' = protected (derivative t)
    it = t' p

    dec 1 = Nothing
    dec k = Just (k-1)

-- | Same as 'evalTermSndDiff' but optimized for intervals 
evalTermSndDiffInterval :: Int -> Int -> [Interval Double] -> Term -> Interval Double
evalTermSndDiffInterval ix iy domains (Term t ks)
  | M.member ix ks' && M.member iy ks = ki*kj*tp''*px'*py' + kij*tp'*pxy'
  | otherwise                         = singleton 0
  where
    ks' = M.update dec iy ks
    ki = singleton . fromIntegral $ ks M.! ix
    kj = singleton . fromIntegral $ ks M.! iy
    kij = kj * singleton (fromIntegral (ks' M.! ix))
    p    = polynomialInterval domains ks
    px'  = polynomialInterval domains (M.update dec ix ks)
    py'  = polynomialInterval domains (M.update dec iy ks)
    pxy' = polynomialInterval domains (M.update dec ix $ M.update dec iy ks)
    t'   = protected (derivative t)
    t''  = protected (sndDerivative t)
    tp'  = t'  p
    tp'' = t'' p

    dec 1 = Nothing
    dec k = Just (k-1)

-- | evaluates an expression by evaluating the terms into a list
-- applying the weight and summing the results.
evalGeneric :: (Dataset Double -> Term -> Column Double) -> Dataset Double -> Expr -> [Double] -> Column Double
evalGeneric f xss terms ws = b + sum weightedTerms
  where
    weightedTerms = zipWith multWeight ws (map (f xss') terms)
    multWeight w  = LA.cmap (w*)
    b             = V.head xss
    xss'          = V.tail xss

-- | Evaluating an expression is simply applying 'evalTerm' 
evalExpr :: Dataset Double -> Expr -> [Double] -> Column Double
evalExpr = evalGeneric evalTerm

-- | Evaluating the derivative is applying 'evalTermDiff' in the expression 
evalDiff :: Int -> Dataset Double -> Expr -> [Double] -> Column Double
evalDiff ix = evalGeneric (evalTermDiff ix)

-- | Evaluating the second derivative is applying 'evalTermSndDiff' in the expression
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
exprToMatrix xss = LA.fromColumns . (V.head xss :) . map (evalTerm (V.tail xss))

-- | Clean the expression by removing the invalid terms
-- TODO: move to IT.Regression 
cleanExpr :: Dataset Double -> [Interval Double] -> Expr -> (Expr, LA.Matrix Double)
cleanExpr xss domains = second (LA.fromColumns . (b:)) . foldr p ([], [])
  where
    xss'           = V.tail xss
    b              = V.head xss
    p t (ts, cols) = if isInvalidInterval (evalTermInterval domains t)
                        then (ts, cols)
                        else (t:ts, evalTerm xss' t : cols)
{-                        
    p' t (ts, cols) = let col = evalTerm xss' t
                      in  if VV.all (not.isInvalid) col
                            then (t:ts, col:cols)
                            else (ts, cols)
-}
isInvalidInterval :: Interval Double -> Bool                        
isInvalidInterval ys = I.null ys 
                     ||  isInfinite ys1 || isInfinite ys2 
                     || ys2 < ys1 
                     || abs ys1 >= 1e150 || abs ys2 >= 1e150
                     || isNaN ys1 || isNaN ys2
                     || width ys < 1e-10
  where
    ys1 = inf ys
    ys2 = sup ys 

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
  | I.null y      = y
  | hasInf x      = nanInterval
  | sup y < inf y = sup y ... inf y
  | otherwise     = y
  where
    y = f x
