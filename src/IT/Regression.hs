--{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
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

import Data.Semigroup

import GHC.Conc (numCapabilities)

import IT
import IT.Algorithms

import qualified Data.Vector.Storable as V
import qualified Data.Vector as VV
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Map.Strict as M

import Control.Parallel.Strategies
import Control.DeepSeq
import Data.List.Split

type Vector = LA.Vector Double

-- * IT specific stuff

-- | IT Instance for regression evals an expression to
-- sum(w_i * term_i) + w0
-- with
--      term_i = f_i(p_i(xs))
--  and p_i(xs) = prod(x^eij)
instance IT Double where
  --itTimes :: Dataset Double -> Interaction -> Column Double
  itTimes vs m
    | VV.length vs == 0 = LA.fromList []
    | otherwise         = M.foldrWithKey f vzero m
    where
      vzero    = n LA.|> repeat 1
      f ix p v = v * (LA.cmap (^^p) (vs VV.! ix))
      n        = LA.size (vs VV.! 0)

  --itAdd :: [Column Double] -> Column Double
  itAdd = getSum . foldMap Sum

  --itWeight :: Double -> Column Double -> Column Double
  itWeight w cs = LA.cmap (w*) cs

-- | Transformation Functions
regSin, regCos, regTan, regTanh, regSqrt, regAbsSqrt, regLog, regExp :: Transformation Double
regSin     = Transformation "sin" sin
regCos     = Transformation "cos" cos
regTan     = Transformation "tan" tan
regTanh    = Transformation "tanh" tanh

regSqrt    = Transformation "sqrt" sqrt
regAbsSqrt = Transformation "sqrt.abs" (sqrt.abs)
regLog     = Transformation "log" log
regExp     = Transformation "exp" exp

regTrig      = [regSin, regCos, regTanh] -- regTan
regNonLinear = [regExp, regLog, regAbsSqrt] -- regSqrt regExp
regLinear    = [Transformation "id" id]

regAll = regTrig ++ regNonLinear ++ regLinear

-- * Error measures

-- | Mean for a vector of doubles
mean :: Vector -> Double
mean xs = V.sum xs / fromIntegral (V.length xs)

-- | Variance for a vector of doubles
var :: Vector -> Double
var xs = sum' / fromIntegral (V.length xs)
  where
    mu   = mean xs
    sum' = V.foldl (\s x -> s + (x-mu)^2) 0 xs

-- | generic mean error measure
meanError :: (Double -> Double) -- ^ a function to be applied to the error terms (abs, square,...)
          -> Vector             -- ^ fitted values
          -> Vector             -- ^ target values
          -> Double
meanError op ysHat ys = mean $ V.map op $ ysHat - ys

-- * Common error measures for regression: 
-- MSE, MAE, RMSE, NMSE, r^2

-- | Mean Squared Error
mse           = meanError (^2)

-- | Mean Absolute Error
mae           = meanError abs

-- | Normalized Mean Squared Error
nmse ysHat ys = mse ysHat ys / var ys

-- | Root of the Mean Squared Error 
rmse ysHat ys = sqrt $ mse ysHat ys

-- | R^2 
rSq ysHat ys  = 1 - r/t
  where
    ym      = mean ys
    t       = sumOfSq $ V.map (\yi -> yi - ym) ys
    r       = sumOfSq $ ys - ysHat
    sumOfSq = V.foldl (\s di -> s + di^2) 0

-- | Predict a linear model
predict :: LA.Matrix Double -> Vector -> Vector
predict xs w = xs LA.#> w

-- | Store regression statitstics
data RegStats = RS { _rmse    :: Double
                   , _mae     :: Double
                   , _nmse    :: Double
                   , _r2      :: Double
                   , _weights :: Vector  -- ^ the fitted weights for the IT expression
                   }


-- | Displaying the statistics, one per line
instance Show RegStats where
  show (RS r m n r2 w) = stats ++ "\n\n"
    where
      stats = unlines $ zipWith (++) names vals
      names = ["RMSE: ", "MAE: ", "NMSE: ", "r^2: ", "Weights: "]
      vals  = [show r, show m, show n, show r2, show w]

-- | Used to deepforce the evaluation
instance NFData RegStats where
  rnf a = ()

-- * Utility functions

-- | A value is invalid if it's wether NaN or Infinite
isInvalid :: Double -> Bool
isInvalid x = isNaN x || isInfinite x

-- | a set of points is valid if none of its values are invalid and
-- the maximum abosolute value is below 1e150 (to avoid overflow)
isValid :: [Double] -> Bool
isValid xs = all (not.isInvalid) xs && (maximum (map abs xs) < 1e150)

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
exprToMatrix :: Dataset Double -> Expr Double -> LA.Matrix Double
exprToMatrix rss (Expr e) = ((1 LA.|||) . LA.fromColumns) zss
  where
    zss = map (`evalTerm` rss) e

-- | Clean the expression by removing the invalid teerms
cleanExpr :: Dataset Double -> Expr Double -> Expr Double
cleanExpr rss (Expr e) = Expr (cleanExpr' e)
  where
    cleanExpr' [] = []
    cleanExpr' (t:e) = if ((/=[]) . LA.find isInvalid . evalTerm t) rss
                       then cleanExpr' e
                       else t : cleanExpr' e

-- | Checks if the fitness of a solution is not Inf nor NaN.
notInfNan :: Solution Double RegStats -> Bool
notInfNan s = not (isInfinite f || isNaN f)
  where f = _fit s

-- | Parallel strategy for evaluating multiple expressions
--parMapChunk :: Int -> (Expr Double -> LA.Matrix Double) -> [Expr Double] -> [LA.Matrix Double]
parMapChunk 0 f xs = map f xs
--parMapChunk n f xs = concatMap (map f) (chunksOf n xs) `using` parList rpar --rdeepseq-- 
parMapChunk n f xs = map f xs `using` parListChunk n rpar -- rpar or rdeepseq

-- | Fitness function for regression
-- 
--  Split the dataset into twice the available cores
--  evaluate the expressions in parallel
--  run a Linear regression on the evaluated expressions
--  Remove from the population any expression that leads to NaNs or Infs
--fitnessReg :: Int -> Dataset Double -> Vector -> [Expr Double] -> Population Double RegStats
fitnessReg :: Dataset Double -> Vector -> Expr Double -> Maybe (Solution Double RegStats)
fitnessReg xss ys expr 
  | notInfNan ps = Just ps
  | otherwise    = Nothing
  where 
    ps = regress ys expr $ exprToMatrix xss expr 
  {-
fitnessReg nPop xss ys []       = []
fitnessReg nPop xss ys exprs = let n  = nPop `div` (2*numCapabilities)
                                   --zs = parMapChunk n (exprToMatrix xss) exprs
                                   ps = parMapChunk n (\e -> regress ys e $ exprToMatrix xss e) exprs
                                   --ps = zipWith (regress ys) exprs zs
                               in  filter notInfNan ps
-}
-- | Evaluates an expression into the test set. This is different from `fitnessReg` since
-- it doesn't apply OLS.
fitnessTest :: Dataset Double -> Vector -> Solution Double RegStats -> RegStats
fitnessTest xss ys sol = let zs = exprToMatrix xss (_expr sol)
                             ws = (_weights . _stat) sol
                             ysHat = predict zs ws
                             rs    = RS (rmse ysHat ys) (mae ysHat ys) (nmse ysHat ys) (rSq ysHat ys) ws
                             inf   = 1/0
                          in if V.length ws == LA.cols zs
                             then rs
                             else RS inf inf inf inf (V.singleton 0.0)


-- | Experimental: round off floating point to the 1e-10 place.
roundoff :: RealFrac a => a -> a
roundoff x
  | x < thr   = 0.0
  | x > 1e200 = x
  | otherwise = (fromInteger $ round (x / thr)) * thr
  where thr = 1e-10

bogusSolution :: RegStats
bogusSolution = RS inf inf inf inf (V.singleton 0.0)
  where inf = 1/0

-- build a RegStats 
buildSolution :: Vector -> Vector -> Vector -> RegStats 
buildSolution ysHat ys = RS (rmse ysHat ys) (mae ysHat ys) (nmse ysHat ys) (rSq ysHat ys)

-- | Solve the OLS *zss*w = ys*
solveOLS :: LA.Matrix Double -> Vector -> Vector
solveOLS zss ys = LA.flatten $ LA.linearSolveSVD zss (LA.asColumn ys)

-- | Applies OLS and returns a Solution
-- if the expression is invalid, it returns Infinity as a fitness
regress :: Vector -> Expr Double -> LA.Matrix Double -> Solution Double RegStats
regress ys expr zss
  | LA.rows zss == 0 || not (all isValid (LA.toLists zss)) 
    = Sol expr (_rmse bogusSolution) bogusSolution
  | otherwise 
    = let ws    = solveOLS zss ys 
          ws'   = V.map roundoff ws
          ysHat = predict zss ws
          ysHat'= predict zss ws'
          rs    = if abs (rmse ysHat' ys - rmse ysHat ys) < 0.01
                   then buildSolution ysHat' ys ws' 
                   else buildSolution ysHat ys ws
      in  Sol expr (_rmse rs ) rs
