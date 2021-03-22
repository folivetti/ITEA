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

import Data.Maybe
import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as LA
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
solveOLS zss ys = LA.flatten $ LA.linearSolveSVD zss (LA.asColumn ys)

-- | Applies OLS and returns a Solution
-- if the expression is invalid, it returns Infinity as a fitness
regress :: Vector -> LA.Matrix Double -> Maybe (Vector, [Vector])
regress ys zss
  | LA.rows zss == 0 || not (all isValid (LA.toLists zss))
    = Nothing
  | otherwise
    = let ws    = solveOLS zss ys
          ysHat = predict zss ws
      in  Just (ysHat, [ws])

classify :: Vector -> LA.Matrix Double -> Maybe (Vector, [Vector])
classify ys zss
  | LA.rows zss == 0 || not (all isValid (LA.toLists zss))
    = Nothing
  | otherwise
    = let ws0     = LA.konst 0 (LA.cols zss)
          (ws, _) = BC.learn (BC.ConjugateGradientPR 0.1 0.1) 0.0001 500 BC.RegNone zss ys ws0
          ysHat   = LM.hypothesis LM.Logistic zss ws
      in  Just (ysHat, [ws])

classifyMult :: Vector -> LA.Matrix Double -> Maybe (Vector, [Vector])
classifyMult ys zss
  | LA.rows zss == 0 || not (all isValid (LA.toLists zss))
    = Nothing
  | otherwise
    = let ws0       = replicate numLabels $ LA.konst 0 (LA.cols zss)
          numLabels = length $ nub $ LA.toList ys
          (ws, _)   = OVA.learn (OVA.ConjugateGradientPR 0.1 0.1) 0.0001 500 OVA.RegNone numLabels zss ys ws0
          ysHat     = OVA.predict zss ws
      in  Just (ysHat, ws)
      
-- | Fitness function for regression
-- 
--  Split the dataset into twice the available cores
--  evaluate the expressions in parallel
--  run a Linear regression on the evaluated expressions
--  Remove from the population any expression that leads to NaNs or Infs
-- it was fitnessReg

evalTrain :: Task -> NonEmpty Measure -> Constraint -> Penalty -> Dataset Double -> Vector -> Expr -> Maybe Solution
evalTrain task measures cnstrFun penalty xss ys expr =

--  | notInfNan ps = Just ps 
--  | otherwise    = Nothing
  case res of
       Nothing -> Nothing
       Just _  -> Just ps 
  where
    zss         = exprToMatrix xss expr
    fitFun      = _fun . NE.head $ measures
    res         = case task of
                    Regression     -> tryToRound (`fitFun` ys) zss <$> regress ys zss
                    Classification -> classify ys zss
                    ClassMult      -> classifyMult ys zss
    (ysHat, ws) = fromJust res
    fit         = NE.toList $ NE.map ((`uncurry` (ysHat, ys)) . _fun) measures
    ws'         = V.toList $ head ws

    len         = exprLength expr ws'
    cnst        = cnstrFun expr ws'
    pnlty       = case penalty of
                    NoPenalty -> 0.0
                    Len c     -> c * fromIntegral len
                    Shape c   -> c*cnst
    ps          = Sol expr fit cnst len pnlty ws



-- | Evaluates an expression into the test set. This is different from `fitnessReg` since
-- it doesn't apply OLS.
-- It was: fitnessTest
evalTest :: Task -> NonEmpty Measure -> Dataset Double -> Vector -> Solution -> Maybe [Double]
evalTest task measures xss ys sol 
  | V.length (head ws) /= LA.cols zss = Nothing
  | otherwise                  = Just fit
  where
    zss   = exprToMatrix xss (_expr sol)
    ws    = _weights sol
    ysHat = case task of
              Regression     -> predict zss (head ws)
              Classification -> LM.hypothesis LM.Logistic zss (head ws)
              ClassMult      -> OVA.predict zss ws   
    fit   = NE.toList $ NE.map ((`uncurry` (ysHat, ys)) . _fun) measures

-- | Experimental: round off floating point to the 1e-10 place.
roundoff :: RealFrac a => a -> a
roundoff x
  | x < thr   = 0.0
  | x > 1e200 = x
  | otherwise = fromInteger (round (x / thr)) * thr
  where thr = 1e-10

-- what to do with you?
tryToRound :: (Vector -> Double) -> LA.Matrix Double -> (Vector, [Vector]) -> (Vector, [Vector])
tryToRound f zss (ysHat, (ws:_)) =
  let ws'         = V.map roundoff ws
      ysHat'      = predict zss ws'
  in  if abs (f ysHat' - f ysHat) < 0.01
          then (ysHat', [ws'])
          else (ysHat, [ws])
tryToRound _ _ _ = error "empty weight list"
