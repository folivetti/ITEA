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
-}
module RunCrossVal where

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V

import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Ord
import Data.Maybe

import Control.Monad
import Control.Monad.State
import System.Random
import System.Random.Shuffle

import IT
import ITEA.Config
import ITEA.Report
import IT.ITEA
import IT.Regression
import IT.Algorithms
import IT.Metrics

-- | Returns the mutation configuration, population size and number of generations.
--
-- * '(e1, e2)' are the minimum and maximum exponentes.
-- * 'tmax' is the maximum number of terms.
-- * 'pop' is the population size.
createMutCfg :: Integral c => (Int, Int) -> Int -> c -> (MutationCfg, c, c)
createMutCfg (e1,e2) tmax pop = (cfg, pop, 100000 `div` pop)
  where cfg = validateConfig
            $  exponents e1 e2
            <> termLimit 2 tmax
            <> nonzeroExps 1
            <> transFunctions [Id, Sin, Cos, Tanh, SqrtAbs, Log, Exp]

-- | Validates the program arguments
validateArgs :: [String] -> (String, Int)
validateArgs (x:y:_) = (x, read y)
validateArgs _ = error "Usage: crossval dataname fold"

-- | Runs a single experiment with a given configuration
runITEARegCV :: Fitness                         -- ^ Training fitness function
             -> (Solution -> Maybe [Double])          -- ^ Test fitness function
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
      result             = fromMaybe [1/0] $ fitTest best
  (return.head) result


-- | runs a configuration for a given data set
runCfg :: String -> Int -> (MutationCfg, Int, Int) -> IO Double
runCfg dname fold (mutCfg, pop, gen) = do
  let fname = "datasets/" ++ dname ++ "/" ++ dname ++ "-train-" ++ show fold ++ ".dat"

  (trainX, trainY) <- parseFile <$> readFile fname
  g <- newStdGen

  let nRows = LA.rows trainX
      dim   = LA.cols trainX

      -- random 5-cv split
      cycle' []     = []
      cycle' (x:xs) = xs ++ [x]
      rndix         = shuffle' [0 .. (nRows-1)] nRows g
      nRows'        = nRows `div` 5
      idxs          = [take nRows' $ drop (i*nRows') rndix | i <- [0..4]]
      folds         = take 5 $ map (\(x:xs) -> (concat xs,x)) $ iterate cycle' idxs

      -- tr = training, tv = validation
      getY is = LA.flatten $ LA.asColumn trainY LA.?? (LA.Pos (LA.idxs is), LA.All)
      getX is = trainX LA.?? (LA.Pos (LA.idxs is), LA.All)

      trYs  = map (getY.fst) folds
      tvYs  = map (getY.snd) folds

      trXs  = map (getX.fst) folds
      tvXs  = map (getX.snd) folds

      toRegMtx = V.fromList . LA.toColumns
      criteria = NE.fromList [_rmse]

      fitTrains = zipWith (\x y  -> evalTrain Regression criteria (toRegMtx x) y) trXs trYs
      fitTests  =  zipWith (\x y -> evalTest Regression criteria (toRegMtx x) y) tvXs tvYs

      average xs = sum xs / fromIntegral (length xs)

      run fitTr fitTe = runITEARegCV fitTr fitTe dim mutCfg pop gen

  rmses <- zipWithM run fitTrains fitTests

  return $ average rmses

-- | Main function
runCrossVal :: [String] -> IO ()
runCrossVal args = do
  let
    -- generate all combination of configurations
    allCfgs        =  [createMutCfg]
                  <*> [(-2,2),(-3,3)]
                  <*> [10,15]
                  <*> [100, 250, 500]

    (dname, nfold) = validateArgs args

  tests <- mapM (runCfg dname nfold) allCfgs

  let (bestCfg, bestRMSE) = minimumBy (comparing snd) (zip allCfgs tests)
  print $ dname ++ "," ++ show nfold ++ "," ++ show bestCfg ++ "," ++ show bestRMSE
