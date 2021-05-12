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
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Storable as V
import qualified Data.Vector as VV
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
          -> Expr
          -> Expr
          -> Maybe Solution
evalTrain task measures cnstrFun penalty xss_train ys_train xss_val ys_val exprP exprQ
  | null exprP' = Nothing 
  | otherwise   = Just $ Sol exprP' exprQ' fit cnst len pnlty ws
  where
    -- Fit the rational IT 
    (exprP', zssP) = cleanExpr xss_train exprP 
    (exprQ', zssQ) = second (createQ ys_train) $ cleanExpr xss_train exprQ
    zss            = LA.fromColumns (zssP <> map negate zssQ)
    ws             = fitTask task zss ys_train

    -- Validate
    np                     = length exprP' + 1
    wsP                    = map (V.take np) ws
    wsQ                    = map (V.cons 1.0 . V.drop np) ws
    (zss_val_P, zss_val_Q) = applyBoth (LA.fromColumns . exprToMatrix xss_val) exprP' exprQ'
    ysHat_P                = predictTask task zss_val_P wsP
    ysHat_Q                = predictTask task zss_val_Q wsQ
    ysHat                  = ysHat_P / ysHat_Q
    fit                    = applyMeasures measures ysHat ys_val

    -- Len and constraint
    ws'   = V.toList $ head ws
    len   = exprLength exprP' (take np ws') + exprLength exprQ' (drop np ws')
    cnst  = cnstrFun exprP' ws'
    pnlty = evalPenalty penalty len cnst

    createQ ys      = map (*ys) . tail
    applyBoth f x y = (f x, f y)

-- | Evaluates an expression into the test set. This is different from `fitnessReg` since
-- it doesn't apply OLS.
-- It was: fitnessTest
evalTest :: Task -> NonEmpty Measure -> Dataset Double -> Vector -> Solution -> Maybe [Double]
evalTest task measures xss ys sol
  | V.length (head wsP) /= LA.cols zssP = Nothing
  | otherwise                           = Just fit
  where
    exprP  = _expr sol
    exprQ  = _ratio sol
    np     = length exprP + 1
    wsP    = map (V.take np) $ _weights sol
    wsQ    = map (V.cons 1.0 . V.drop np) $ _weights sol
    zssP   = LA.fromColumns $ exprToMatrix xss exprP
    zssQ   = LA.fromColumns $ exprToMatrix xss exprQ
    ysHatP = predictTask task zssP wsP
    ysHatQ = predictTask task zssQ wsQ
    ysHat  = ysHatP/ysHatQ
    fit    = applyMeasures measures ysHat ys

-- | evaluates an expression to a set of samples 
--
exprToMatrix :: Dataset Double -> Expr -> [Vector]
exprToMatrix xss = (VV.head xss :) . map (evalTerm (VV.tail xss))

-- | Clean the expression by removing the invalid terms
cleanExpr :: Dataset Double -> Expr -> (Expr, [Vector])
cleanExpr xss = second (b:) . foldr p ([], [])
  where
    xss'           = VV.tail xss
    b              = VV.head xss
    p t (ts, cols) = let col = evalTerm xss' t
                     in  if V.all (not.isInvalid) col
                            then (t:ts, col:cols)
                            else (ts, cols)

