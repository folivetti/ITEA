{-|
Module      : CROSSVAL
Description : Hyperparameter tuning for ITEA
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Hyperparameter tuning for ITEA
-}
module Main where

import ITEA.Config
import IT.ITEA
import IT.Regression
import IT.Algorithms

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V

import Data.List
import Data.Ord

import Control.Monad.State
import System.Random.SplitMix
import System.Random.Shuffle
import System.Environment


-- | Creates a mutation configuration instance
createMutCfg (e1,e2) tmax pop = (cfg, pop, 100000 `div` pop)
  where cfg = validateConfig
            $  exponents e1 e2
            <> termLimit 2 tmax
            <> nonzeroExps 1
            <> transFunctions FAll

-- | Validates the program arguments
validateArgs :: [String] -> (String, Int)
validateArgs (x:y:_) = (x, read y)
validateArgs _ = error "Usage: crossval dataname fold"

-- | Runs a single experiment with a given configuration
runITEARegCV :: Fitness Double RegStats                -- ^ Training fitness function
             -> (Solution Double RegStats -> RegStats) -- ^ Test fitness function
             -> Int                                                 -- ^ Problem dimension
             -> MutationCfg                                         -- ^ Mutation configuration
             -> Int                                                 -- ^ population size
             -> Int                                                 -- ^ number of generations
             -> IO Double
runITEARegCV fitTrain fitTest dim mcfg nPop nGens = do
  -- random seed
  g <- initSMGen

  -- run ITEA with given configuration
  let (mutFun, rndTerm)  = withMutation mcfg dim
      p0                 = initialPop dim 4 nPop rndTerm fitTrain
      gens               = (p0 >>= itea mutFun fitTrain) `evalState` g
      best               = getBest nGens gens
  (return._rmse.fitTest)  best

average xs = sum xs / l
  where l = fromIntegral (length xs)
  
cycle' (x:xs) = xs ++ [x]

-- | runs a configuration for a given data set
runCfg :: String -> Int -> (MutationCfg, Int, Int) -> IO Double
runCfg dname fold (mutCfg, pop, gen) = do
  let fname = "datasets/" ++ dname ++ "/" ++ dname ++ "-train-" ++ show 0 ++ ".dat" 
  
  (trainX, trainY) <- parseFile <$> readFile fname
  g <- initSMGen
  
  let nRows = LA.rows trainX
      dim   = LA.cols trainX
      
      -- random 5-cv split
      rndix  = shuffle' [0 .. (nRows-1)] nRows g
      nRows' = nRows `div` 5
      idxs   = [take nRows' $ drop (i*nRows') rndix | i <- [0..4]]
      folds  = take 5 $ map (\(x:xs) -> (concat xs,x)) $ iterate cycle' idxs
      --idxs1 = take (nRows `div` 2) rndix
      --idxs2 = drop (nRows `div` 2) rndix
      
      -- tr = training, tv = validation
      getY is = LA.flatten $ (LA.asColumn trainY) LA.?? (LA.Pos (LA.idxs is), LA.All)
      getX is = trainX LA.?? (LA.Pos (LA.idxs is), LA.All)
      
      trYs  = map (getY.fst) folds
      tvYs  = map (getY.snd) folds
      
      trXs  = map (getX.fst) folds
      tvXs  = map (getX.snd) folds
      
      --trY   = LA.flatten $ (LA.asColumn trainY) LA.?? (LA.Pos (LA.idxs idxs1), LA.All)
      --tvY   = LA.flatten $ (LA.asColumn trainY) LA.?? (LA.Pos (LA.idxs idxs2), LA.All)
      --trX   = trainX LA.?? (LA.Pos (LA.idxs idxs1), LA.All)
      --tvX   = trainX LA.?? (LA.Pos (LA.idxs idxs2), LA.All)
      
      toRegMtx = V.fromList . LA.toColumns
      
      fitTrains = zipWith (\x y -> fitnessReg pop (toRegMtx x) y) trXs trYs
      fitTests  =  zipWith (\x y -> fitnessTest (toRegMtx x) y) tvXs tvYs
      --fitTrain1 = fitnessReg pop (toRegMtx trX) trY          
      --fitTest1  = fitnessTest (toRegMtx tvX) tvY
      
      --fitTrain2 = fitnessReg pop (toRegMtx tvX) tvY
      --fitTest2  = fitnessTest (toRegMtx trX) trY
      
      --runFive = sequence . replicate 5
      
      run fitTr fitTe = runITEARegCV fitTr fitTe dim mutCfg pop gen
      
  rmses <- sequence $ zipWith run fitTrains fitTests
  
  --rmses1 <- runFive (runITEARegCV fitTrain1 fitTest1 dim mutCfg pop gen)
  --rmses2 <- runFive (runITEARegCV fitTrain2 fitTest2 dim mutCfg pop gen)

  --return $ (sum rmses1 + sum rmses2) / 10.0
  return $ average rmses
  
-- | Main function
main :: IO ()
main = do
  args <- getArgs  
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
