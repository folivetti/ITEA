{-|
Module      : CROSSVAL
Description : Hyperparameter tuning for ITEA
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Run a 5-fold cross-validation on the training set to find the best
combination of hyperparameters.

TODO: refactor, document, write the best configuration in a config file
-}
module RunCrossVal where

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V
import qualified Numeric.Morpheus.MatrixReduce as MTX
import Numeric.Interval ((...))

import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Ord
import Data.Maybe

import Control.Monad
import Control.Monad.State
import Data.Bifunctor
import System.Random
import System.Random.Shuffle
import Data.ConfigFile
import Data.Either.Utils

import IT
import IT.Shape
import ITEA.Config
import ITEA.Report
import IT.ITEA
import IT.FI2POP
import IT.Regression
import IT.Algorithms
import IT.Metrics

import RunConfig (Alg(..),getSetting,getWithDefault)

-- | Returns the mutation configuration, population size and number of generations.
--
-- * '(e1, e2)' are the minimum and maximum exponentes.
-- * 'tmax' is the maximum number of terms.
-- * 'pop' is the population size.
createMutCfg :: (Int, Int) -> Int -> [Transformation] -> MutationCfg
createMutCfg (e1,e2) tmax tfuncs = cfg
  where cfg = validateConfig
            $  exponents e1 e2
            <> termLimit 2 tmax
            <> nonzeroExps 1
            <> transFunctions tfuncs -- [Id, Sin, Cos, Tanh, SqrtAbs, Log, Exp]
            <> measures ["NMSE"]

-- | Validates the program arguments
validateArgs :: [String] -> (String, Int)
validateArgs (x:y:_) = (x, read y)
validateArgs _ = error "Usage: crossval dataname fold"

-- | Runs a single experiment with a given configuration
runITEARegCV :: Fitness                                             -- ^ Training fitness function
             -> (Solution -> Maybe [Double])                        -- ^ Test fitness function
             -> Int                                                 -- ^ Problem dimension
             -> MutationCfg                                         -- ^ Mutation configuration
             -> Int                                                 -- ^ population size
             -> Int                                                 -- ^ number of generations
             -> IO Double
runITEARegCV fitTrain fitTest dim mcfg nPop nGens = do
  -- random seed
  g <- newStdGen

  -- run ITEA with given configuration
  let (mutFun, rndTerm)  = withMutation mcfg dim
      p0                 = initialPop 4 nPop rndTerm fitTrain 
      gens               = (p0 >>= itea mutFun fitTrain) `evalState` g
      best               = getBest nGens gens
      result             = fromMaybe [1e+10] $ fitTest best
  (return.head) result

runFI2POPRegCV :: Fitness                                           -- ^ Training fitness function
             -> (Solution -> Maybe [Double])                        -- ^ Test fitness function
             -> Int                                                 -- ^ Problem dimension
             -> MutationCfg                                         -- ^ Mutation configuration
             -> Int                                                 -- ^ population size
             -> Int                                                 -- ^ number of generations
             -> IO Double
runFI2POPRegCV fitTrain fitTest dim mcfg nPop nGens = do
  -- random seed
  g <- newStdGen

  -- run ITEA with given configuration
  let (mutFun, rndTerm)  = withMutation mcfg dim
      p0                 = splitPop <$> initialPop 4 nPop rndTerm fitTrain 
      gens               = map fst $ (p0 >>= fi2pop mutFun fitTrain) `evalState` g
      mBest              = getBestMaybe nGens gens
      result             = case mBest of
                                Nothing   -> [1e+10]
                                Just best -> fromMaybe [1e+10] $ fitTest best
  (return.head) result

-- | runs a configuration for a given data set
runCfg :: FilePath -> Int -> MutationCfg -> IO Double
runCfg fname fold mutCfg = do

  cp <- forceEither <$> readfile emptyCP fname
  let
    trainname          = getSetting cp "IO"   "train"

    --nPop               = getSetting cp "Algorithm"   "npop"
    --nGens              = getSetting cp "Algorithm"   "ngens"
    alg                = getSetting cp "Algorithm"   "algorithm"

    -- penalty            = getWithDefault NoPenalty cp "Constraints" "penalty"
    shapes             = getWithDefault [] cp "Constraints" "shapes"
    domains            = getWithDefault Nothing cp "Constraints" "domains"

  (trainX, trainY) <- first (1.0 LA.|||) . parseFile <$> readFile trainname
  g <- newStdGen

  let nRows = LA.rows trainX
      dim   = LA.cols trainX - 1

      minX = Prelude.tail $ LA.toList $ MTX.columnPredicate min trainX
      maxX = Prelude.tail $ LA.toList $ MTX.columnPredicate max trainX
      domains' = case domains of
                      Nothing -> zipWith (...) minX maxX
                      Just ds -> map (uncurry (...)) ds

      -- random 5-cv split
      cycle' []     = []
      cycle' (x:xs) = xs ++ [x]
      rndix         = shuffle' [0 .. (nRows-1)] nRows g
      nRows'        = nRows `div` fold
      idxs          = [take nRows' $ drop (i*nRows') rndix | i <- [0 .. fold-1]]
      folds         = take fold $ map (\(x:xs) -> (concat xs,x)) $ iterate cycle' idxs

      -- tr = training, tv = validation
      getY is = LA.flatten $ LA.asColumn trainY LA.?? (LA.Pos (LA.idxs is), LA.All)
      getX is = trainX LA.?? (LA.Pos (LA.idxs is), LA.All)

      trYs  = map (getY.fst) folds
      tvYs  = map (getY.snd) folds

      trXs  = map (getX.fst) folds
      tvXs  = map (getX.snd) folds

      toRegMtx = V.fromList . LA.toColumns
      criteria = NE.fromList [_nmse]

      fitTrains = zipWith (\x y  -> evalTrain Regression criteria (fromShapes shapes domains) NoPenalty (toRegMtx x) y (toRegMtx x) y domains') trXs trYs

      fitTests  =  zipWith (\x y -> evalTest Regression criteria (toRegMtx x) y) tvXs tvYs

      average xs = sum xs / fromIntegral (length xs)
      {-
      standard xs = average $ map (\x -> (x - x_m)^2) xs
        where x_m = average xs

      confidence xs n = x_m + 1.96 * sqrt (x_s / fromIntegral n)
        where x_m = average xs
              x_s = standard xs
      -}
      run fitTr fitTe = 
        case alg of
          ITEA   -> runITEARegCV fitTr fitTe dim mutCfg 100 100
          FI2POP -> runFI2POPRegCV fitTr fitTe dim mutCfg 100 100

  rmses <- zipWithM run fitTrains fitTests 

  return $ average rmses

-- | Main function
runCrossVal :: [String] -> IO ()
runCrossVal args = do
  let
    -- generate all combination of configurations
    allCfgs        =  [createMutCfg]
                  <*> [(-1,1),(-3,3),(-5,5),(0,1),(0,3),(0,5)]
                  <*> [2,5,10]
                  <*> [[Id],[Id, SqrtAbs, Log, Exp], [Id, SqrtAbs, Log, Exp, Sin, Tanh]] -- , SqrtAbs, Log, Exp]

    (dname, nfold) = validateArgs args

  tests <- mapM (runCfg dname nfold) allCfgs

  let (bestCfg, bestRMSE) = minimumBy (comparing snd) (zip allCfgs tests)
  print $ dname ++ "," ++ show nfold ++ "," ++ show bestCfg ++ "," ++ show bestRMSE
