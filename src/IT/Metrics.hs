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

import Data.Semigroup

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
    sum' = V.foldl (\s x -> s + (x-mu)^(2 :: Int)) 0 xs

-- | generic mean error measure
meanError :: (Vector -> Vector) -- ^ a function to be applied to the error terms (abs, square,...)
          -> Vector             -- ^ fitted values
          -> Vector             -- ^ target values
          -> Double
meanError op ysHat ys = mean $ op $ ysHat - ys

-- * Common error measures for regression: 
-- MSE, MAE, RMSE, NMSE, r^2

-- | Mean Squared Error
mse :: Vector -> Vector -> Double
mse           = meanError (^(2 :: Int))

-- | Mean Absolute Error
mae :: Vector -> Vector -> Double
mae           = meanError abs

-- | Normalized Mean Squared Error
nmse :: Vector -> Vector -> Double
nmse ysHat ys = mse ysHat ys / var ys

-- | Root of the Mean Squared Error 
rmse :: Vector -> Vector -> Double
rmse ysHat ys = sqrt $ mse ysHat ys

-- | negate R^2 - minimization metric
rSq :: Vector -> Vector -> Double
rSq ysHat ys  = negate (1 - r/t)
  where
    ym      = mean ys
    t       = sumOfSq $ V.map (\yi -> yi - ym) ys
    r       = sumOfSq $ ys - ysHat
    sumOfSq = V.foldl (\s di -> s + di^(2 :: Int)) 0

-- * Regression measures 
_rmse, _mae, _nmse, _r2 :: Measure
_rmse = Measure "RMSE" rmse
_mae  = Measure "MAE" mae
_nmse = Measure "NMSE" nmse
_r2   = Measure "R^2" rSq

-- * Classification measures 
_accuracy,_recall,_precision,_f1,_logloss :: Measure
_accuracy  = Measure "Accuracy" accuracy
_recall    = Measure "Recall" recall
_precision = Measure "Precision" precision
_f1        = Measure "F1" f1
_logloss   = Measure "Log-Loss" logloss

-- | Accuracy
accuracy :: Vector -> Vector -> Double
accuracy ysHat ys = -equals/tot
  where
    ys'    = map round $ LA.toList ys
    ysHat' = map round $ LA.toList ysHat
    (Sum equals, Sum tot) = foldMap cmp $ zip ysHat' ys'
    cmp :: (Integer, Integer) -> (Sum Double, Sum Double)
    cmp (yH, y)
      | yH == y   = (Sum 1, Sum 1)
      | otherwise = (Sum 0, Sum 1)

precision :: Vector -> Vector -> Double
precision ysHat ys = equals/tot
  where
    ys'    = map round $ LA.toList ys
    ysHat' = map round $ LA.toList ysHat
    (Sum equals, Sum tot) = foldMap cmp $ zip ysHat' ys'
    cmp :: (Integer, Integer) -> (Sum Double, Sum Double)
    cmp (1, 1)  = (Sum 1, Sum 1)
    cmp (1, 0)  = (Sum 0, Sum 1)
    cmp (_, _) = (Sum 0, Sum 0)

recall :: Vector -> Vector -> Double
recall ysHat ys = equals/tot
  where
    ys'    = map round $ LA.toList ys
    ysHat' = map round $ LA.toList ysHat
    (Sum equals, Sum tot) = foldMap cmp $ zip ysHat' ys'

    cmp :: (Integer, Integer) -> (Sum Double, Sum Double)
    cmp (1, 1)  = (Sum 1, Sum 1)
    cmp (0, 1)  = (Sum 0, Sum 1)
    cmp (_, _) = (Sum 0, Sum 0)

f1 :: Vector -> Vector -> Double
f1 ysHat ys = 2*prec*rec/(prec+rec)
  where
    prec = precision ysHat ys
    rec  = recall ysHat ys

logloss :: Vector -> Vector -> Double
logloss ysHat ys = mean $ -(ys * log ysHat' + (1 - ys)*log(1 - ysHat'))
  where
    ysHat' = LA.cmap (min (1.0 - 1e-15) . max 1e-15) ysHat



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
