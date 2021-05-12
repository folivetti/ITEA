{-# LANGUAGE FunctionalDependencies #-}
{-|
Module      : Example.Regression
Description : Example of usage for Symbolic Regression
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Configuration parsing and report generation.
-}
module ITEA.Config where

import IT
import IT.Algorithms
import IT.Mutation
import IT.Metrics
import IT.Random

import qualified Numeric.LinearAlgebra as LA
import qualified MachineLearning as ML
import Data.List.Split (splitOn)

-- | Class of types that can be validate
class Monoid a => Valid a b | a -> b, b -> a where
  validateConfig :: a -> b

-- | A parameter is either empty (None) or Has something
data Param a = None | Has a deriving Show

-- | Extract parameter. This is a partial function.
fromParam :: Param a -> a
fromParam (Has x) = x
fromParam None    = error "fromParam: empty value"

instance Semigroup (Param a) where
  p <> None = p
  _ <> p    = p
instance Monoid (Param a) where
  mempty = None

-- | Unchecked mutation config 
data UncheckedMutationCfg = UMCfg { _expLim   :: Param (Int, Int)
                                  , _termLim  :: Param (Int, Int)
                                  , _nzExp    :: Param Int
                                  , _transFun :: Param [Transformation]
                                  , _measures :: Param [String]
                                  }

-- | Validated mutation config 
data MutationCfg = MCfg (Int, Int) (Int, Int) Int [Transformation] [String] deriving Show

instance Semigroup UncheckedMutationCfg where
  (UMCfg p1 p2 p3 p4 p5) <> (UMCfg q1 q2 q3 q4 q5) = UMCfg (p1<>q1) (p2<>q2) (p3<>q3) (p4<>q4) (p5<>q5)
instance Monoid UncheckedMutationCfg where
  mempty = UMCfg mempty mempty mempty mempty mempty

-- | Generates a configuration with only '_expLim' holding a value.
exponents :: Int -> Int -> UncheckedMutationCfg
exponents x y = mempty { _expLim   = Has (x,y) }

-- | Generates a configuration with only '_termLim' holding a value.
termLimit :: Int -> Int -> UncheckedMutationCfg
termLimit   x y = mempty { _termLim  = Has (x,y) }

-- | Generates a configuration with only '_nzExp' holding a value.
nonzeroExps :: Int -> UncheckedMutationCfg
nonzeroExps x = mempty { _nzExp = Has x }

-- | Generates a configuration with only '_transFun' holding a value.
transFunctions :: [Transformation] -> UncheckedMutationCfg
transFunctions  fs  = mempty { _transFun = Has fs }

-- | Generates a configuration with only '_measures' holding a value.
measures :: [String] -> UncheckedMutationCfg
measures ms = mempty { _measures = Has ms }

instance Valid UncheckedMutationCfg MutationCfg where
  -- validateConfig :: UncheckedMutationCfg -> MutationCfg
  validateConfig (UMCfg None _ _ _ _) = error "No exponent limits set"
  validateConfig (UMCfg _ None _ _ _) = error "No expression size limits set"
  validateConfig (UMCfg _ _ None _ _) = error "No maximum non-zero exponents set"
  validateConfig (UMCfg _ _ _ None _) = error "No transformation functions chosen"
  validateConfig (UMCfg _ _ _ (Has []) _) = error "No transformation functions chosen"
  validateConfig (UMCfg _ _ _ _ None) = error "No error functions chosen"
  validateConfig (UMCfg _ _ _ _ (Has [])) = error "No error functions chosen"
  validateConfig c = MCfg (pexpLim c) (ptermLim c) (pnzExp c) (ptransFun c) (pmeasure c)
    where
      pexpLim   = fromParam . _expLim
      ptermLim  = fromParam . _termLim
      pnzExp    = fromParam . _nzExp
      ptransFun = fromParam . _transFun
      pmeasure  = fromParam . _measures

getMaxTerms :: MutationCfg -> Int
getMaxTerms (MCfg _ (_, maxTerms) _ _ _) = maxTerms

getMeasure :: MutationCfg -> [Measure]
getMeasure  (MCfg _ _ _ _ m) = map toMeasure m

-- | Parse a numerical csv file into predictors and target variables
parseFile :: String -> (LA.Matrix Double, Vector)
parseFile css = ML.splitToXY . LA.fromLists $ map (map read) dat
  where
    dat = map (splitOn ",") $ lines css

-- | Creates the mutation function and also returns the random term generator (for initialization)
withMutation :: MutationCfg -> Int -> (Mutation, Rnd Term)
withMutation (MCfg elim tlim nzExp transfun _) dim = (mutFun dim elim tlim rndTerm rndTrans, rndTerm)
  where
    (minExp, maxExp) = elim
    rndInter = sampleInterMax dim nzExp minExp maxExp
    rndTrans = sampleTrans transfun -- (map read transfun)
    rndTerm  = sampleTerm rndTrans rndInter

-- * Datasets configuration

data UncheckedDatasets = UD { _trainset :: Param String, _testset :: Param String }  deriving Show
data Datasets = D String String deriving Show

-- | sets the training and test data set names 
trainingset, testset :: String -> UncheckedDatasets
trainingset name = mempty { _trainset = Has name }
testset     name = mempty { _testset = Has name }

instance Semigroup UncheckedDatasets where
  (UD p1 p2) <> (UD q1 q2) = UD (p1<>q1) (p2<>q2)
instance Monoid UncheckedDatasets where
  mempty = UD mempty mempty

instance Valid UncheckedDatasets Datasets where
  validateConfig (UD None _) = error "No training data was set"
  validateConfig (UD _ None) = error "No test data was set"
  validateConfig (UD tr te) = D (fromParam tr) (fromParam te)
