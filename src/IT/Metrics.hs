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

module IT.Metrics where

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector.Storable as V

type Vector = LA.Vector Double
-- * Performance measures

data Measure = Measure { _name :: String
                       , _fun  :: Vector -> Vector -> Double -- ^ true -> predicted -> measure 
                       }

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
mse :: Vector -> Vector -> Double
mse           = meanError (^2)

-- | Mean Absolute Error
mae :: Vector -> Vector -> Double
mae           = meanError abs

-- | Normalized Mean Squared Error
nmse :: Vector -> Vector -> Double
nmse ysHat ys = mse ysHat ys / var ys

-- | Root of the Mean Squared Error 
rmse :: Vector -> Vector -> Double
rmse ysHat ys = sqrt $ mse ysHat ys

-- | R^2 
rSq :: Vector -> Vector -> Double
rSq ysHat ys  = 1 - r/t
  where
    ym      = mean ys
    t       = sumOfSq $ V.map (\yi -> yi - ym) ys
    r       = sumOfSq $ ys - ysHat
    sumOfSq = V.foldl (\s di -> s + di^2) 0

-- * Regression measures 
_rmse, _mae, _nmse, _r2 :: Measure
_rmse = Measure "RMSE" rmse
_mae  = Measure "MAE" mae
_nmse = Measure "NMSE" nmse
_r2   = Measure "R^2" rSq

-- * Classification measures 
_accuracy,_recall,_precision,_f1,_logloss :: Measure
_accuracy  = Measure "Accuracy" undefined
_recall    = Measure "Recall" undefined
_precision = Measure "Precision" undefined
_f1        = Measure "F1" undefined
_logloss   = Measure "Log-Loss" undefined

-- | List of all measures
measureAll :: [Measure]
measureAll = [_rmse, _mae, _nmse, _r2
             , _accuracy, _recall, _precision, _f1, _logloss
             ]

-- | Read a string into a measure
toMeasure :: String -> Measure
toMeasure input
  | null cmp  = error ("Invalid measure: " ++ input)
  | otherwise = (snd.head) cmp
  where
    cmp                       = filter fst $ map isThis measureAll
    isThis m@(Measure name _) = (name == input, m)
