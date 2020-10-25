{-# LANGUAGE TypeFamilies #-}
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
module IT.Regression where

import IT
import IT.Algorithms
import IT.Eval
import IT.Metrics

import Data.List.NonEmpty as NE
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as LA

-- * IT specific stuff
data Task = Regression | Classification

type FitFun = Vector -> Vector -> Double

-- | Predict a linear model
predict :: LA.Matrix Double -> Vector -> Vector
predict xs w = xs LA.#> w

-- | Solve the OLS *zss*w = ys*
solveOLS :: LA.Matrix Double -> Vector -> Vector
solveOLS zss ys = LA.flatten $ LA.linearSolveSVD zss (LA.asColumn ys)

-- | Applies OLS and returns a Solution
-- if the expression is invalid, it returns Infinity as a fitness
regress :: Vector -> Expr Double -> LA.Matrix Double -> (Vector, Vector)
regress ys expr zss
  | LA.rows zss == 0 || not (all isValid (LA.toLists zss))
    = (1/0, 1/0)
  | otherwise
    = let ws    = solveOLS zss ys
          ysHat = predict zss ws
      in  (ysHat, ws)

classify :: Vector -> Expr Double -> LA.Matrix Double -> Solution Double
classify = undefined

-- | Fitness function for regression
-- 
--  Split the dataset into twice the available cores
--  evaluate the expressions in parallel
--  run a Linear regression on the evaluated expressions
--  Remove from the population any expression that leads to NaNs or Infs
-- it was fitnessReg
evalTrain :: NonEmpty Measure -> Dataset Double -> Vector -> Expr Double -> Maybe (Solution Double)
evalTrain measures xss ys expr
  | notInfNan ps = Just ps 
  | otherwise    = Nothing
  where
    zss         = exprToMatrix xss expr
    fitFun      = _fun . NE.head $ measures
    (ysHat, ws) = tryToRound (fitFun ys) zss $ regress ys expr zss
    fit         = NE.map ((`uncurry` (ys, ysHat)) . _fun) measures
    ps          = Sol expr fit ws


-- | Evaluates an expression into the test set. This is different from `fitnessReg` since
-- it doesn't apply OLS.
-- It was: fitnessTest
evalTest :: NonEmpty Measure -> Dataset Double -> Vector -> Solution Double -> Maybe (NonEmpty Double)
evalTest measures xss ys sol 
  | V.length ws /= LA.cols zss = Nothing
  | otherwise                  = Just fit
  where
    zss   = exprToMatrix xss (_expr sol)
    ws    = _weights sol
    ysHat = predict zss ws
    fit   = NE.map ((`uncurry` (ys, ysHat)) . _fun) measures

-- | Experimental: round off floating point to the 1e-10 place.
roundoff :: RealFrac a => a -> a
roundoff x
  | x < thr   = 0.0
  | x > 1e200 = x
  | otherwise = fromInteger (round (x / thr)) * thr
  where thr = 1e-10

-- what to do with you?
tryToRound f zss (ysHat, ws) =
  let ws'         = V.map roundoff ws
      ysHat'      = predict zss ws'
  in  if abs (f ysHat' - f ysHat) < 0.01
          then (ysHat', ws')
          else (ysHat, ws)

