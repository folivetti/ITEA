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

import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as LA
import Numeric.Interval (Interval)
import qualified MachineLearning.Classification.Binary as BC
import qualified MachineLearning.LogisticModel as LM
import qualified MachineLearning.Classification.OneVsAll as OVA

-- * IT specific stuff
data Task = Regression | Classification | ClassMult
         deriving (Eq, Read)

type FitFun = Vector -> Vector -> Double

data Penalty = NoPenalty | Len Double | Shape Double deriving (Show, Read)


-- | Predict a linear model
predict :: LA.Matrix Double -> Vector -> Vector
predict xs w = xs LA.#> w

-- | Solve the OLS *zss*w = ys*
solveOLS :: LA.Matrix Double -> Vector -> Vector
solveOLS zss = LA.flatten . LA.linearSolveSVD zss . LA.asColumn

isInvalidMatrix :: LA.Matrix Double -> Bool
isInvalidMatrix zss = LA.rows zss == 0 || V.any isInvalid (LA.flatten zss)

-- | Applies OLS and returns a Solution
-- if the expression is invalid, it returns Infinity as a fitness
regress :: LA.Matrix Double -> Vector -> [Vector]
regress zss ys = [solveOLS zss ys]

classify :: LA.Matrix Double -> Vector -> [Vector]
classify zss ys
    = let ws0     = LA.konst 0 (LA.cols zss)
          (ws, _) = BC.learn (BC.ConjugateGradientPR 0.1 0.1) 0.0001 500 BC.RegNone zss ys ws0
      in  [ws]

classifyMult :: LA.Matrix Double -> Vector -> [Vector]
classifyMult zss ys
    = let ws0       = replicate numLabels $ LA.konst 0 (LA.cols zss)
          numLabels = length $ nub $ LA.toList ys
          (ws, _)   = OVA.learn (OVA.ConjugateGradientPR 0.1 0.1) 0.0001 500 OVA.RegNone numLabels zss ys ws0
      in  ws

-- | Fitness function for regression
-- 
--  Split the dataset into twice the available cores
--  evaluate the expressions in parallel
--  run a Linear regression on the evaluated expressions
--  Remove from the population any expression that leads to NaNs or Infs
-- it was fitnessReg

fitTask :: Task -> LA.Matrix Double -> Vector -> [Vector]
fitTask Regression     = regress
fitTask Classification = classify
fitTask ClassMult      = classifyMult

predictTask :: Task -> LA.Matrix Double -> [Vector] -> Vector
predictTask _ _ []                   = error "predictTask: empty coefficients matrix"
predictTask Regression zss (w:_)     = predict zss w
predictTask Classification zss (w:_) = LM.hypothesis LM.Logistic zss w
predictTask ClassMult zss ws         = OVA.predict zss ws

evalPenalty :: Penalty -> Int -> Double -> Double
evalPenalty NoPenalty _   _   = 0.0
evalPenalty (Len c)   len _   = fromIntegral len * c
evalPenalty (Shape c) _   val = val*c

applyMeasures :: NonEmpty Measure -> Vector -> Vector -> [Double]
applyMeasures measures ysHat ys = NE.toList $ NE.map ((`uncurry` (ysHat, ys)) . _fun) measures

evalTrain :: Task
          -> NonEmpty Measure
          -> Constraint
          -> Penalty
          -> Dataset Double
          -> Vector
          -> Dataset Double
          -> Vector
          -> [Interval Double]
          -> Expr
          -> Maybe Solution
evalTrain task measures cnstrFun penalty xss_train ys_train xss_val ys_val domains expr
  | null expr' = Nothing 
--  | LA.rank zss < 1 = error $ show expr' <> show zss <> show (map (evalTermInterval domains) expr') <> show domains
  | otherwise  = Just $ Sol expr' fit cnst len pnlty ws
  where
    ws    = fitTask task zss ys_train
    ysHat = predictTask task zss_val ws
    fit   = applyMeasures measures ysHat ys_val
    ws'   = V.toList $ head ws
    len   = exprLength expr' ws'
    cnst  = cnstrFun expr' ws'
    pnlty = evalPenalty penalty len cnst

    (expr', zss) = cleanExpr xss_train domains expr
    zss_val      = exprToMatrix xss_val expr'

-- | Evaluates an expression into the test set. This is different from `fitnessReg` since
-- it doesn't apply OLS.
-- It was: fitnessTest
evalTest :: Task -> NonEmpty Measure -> Dataset Double -> Vector -> Solution -> Maybe [Double]
evalTest task measures xss ys sol
  | V.length (head ws) /= LA.cols zss = Nothing
  | otherwise                         = Just fit
  where
    zss   = exprToMatrix xss (_expr sol)
    ws    = _weights sol
    ysHat = predictTask task zss ws 
    fit   = applyMeasures measures ysHat ys 

{-
-- | Experimental: round off floating point to the 1e-10 place.
roundoff :: RealFrac a => a -> a
roundoff x
  | abs x < thr   = 0.0
  | abs x > 1e200 = x
  | otherwise = fromInteger (round (x / thr)) * thr
  where thr = 1e-15

-- what to do with you?
tryToRound :: (Vector -> Double) -> LA.Matrix Double -> (Vector, [Vector]) -> (Vector, [Vector])
tryToRound f zss (ysHat, (ws:_)) =
  let ws'         = V.map roundoff ws
      ysHat'      = predict zss ws'
  in  (ysHat', [ws']) --if abs (f ysHat' - f ysHat) < 0.01
          --then (ysHat', [ws'])
          --else (ysHat, [ws])
tryToRound _ _ _ = error "empty weight list"
-}
