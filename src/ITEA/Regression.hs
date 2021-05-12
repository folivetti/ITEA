{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Example.Regression
Description : Example of usage for Symbolic Regression
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Function to execute ITEA
-}
module ITEA.Regression where

import IT 
import IT.Random 
import IT.Algorithms
import IT.ITEA
import IT.RITEA
import IT.FI2POP
import IT.Regression
import IT.Shape

import ITEA.Config
import ITEA.Report

import Data.List.NonEmpty hiding (map)

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V

import Control.Monad.State
import System.Random

type AlgRunner = Rnd Population -> RatioMutation -> Fitness -> StdGen -> [Population]

readAndParse :: FilePath -> IO (LA.Matrix Double, Column Double)
readAndParse f = do (xss, ys) <- parseFile <$> readFile f
                    return (1.0 LA.||| xss, ys)

toVecOfColumns :: LA.Matrix Double -> Dataset Double
toVecOfColumns = V.fromList . LA.toColumns

takeNRows, dropNRows :: Int -> LA.Matrix Double -> LA.Matrix Double
takeNRows n xss = xss LA.?? (LA.Take n, LA.All)
dropNRows n xss = xss LA.?? (LA.Drop n, LA.All)

splitValidation :: Double -> LA.Matrix Double -> LA.Vector Double 
                -> (Dataset Double, Column Double, Dataset Double, Column Double)
splitValidation ratio xss ys
  | nRows <= 20 = (toVecOfColumns xss, ys, toVecOfColumns xss, ys)
  | otherwise    = (xss_train, y_train, xss_val, y_val)
  where
    nRows      = LA.rows xss
    nRowsTrain = round (fromIntegral nRows * ratio)
    nRowsVal   = nRows - nRowsTrain
    xss_train  = toVecOfColumns $ takeNRows nRowsTrain xss
    xss_val    = toVecOfColumns $ dropNRows nRowsTrain xss
    y_train    = LA.subVector 0 nRowsTrain ys
    y_val      = LA.subVector nRowsTrain nRowsVal ys

-- | Support function for running ITEA
run :: AlgRunner
    -> Datasets     -- training and test datasets
    -> MutationCfg  -- configuration of mutation operators
    -> Maybe MutationCfg  -- configuration of mutation operators for the ratio expression 
    -> Output       -- output to Screen | PartialLog filename | FullLog filename
    -> Int          -- population size
    -> Int          -- generations
    -> Task
    -> Penalty
    -> [Shape]
    -> Domains
    -> IO ()
run alg (D tr te) mcfg mcfgR output nPop nGens task penalty shapes domains =
 do g <- newStdGen
    (trainX, trainY) <- readAndParse tr
    (testX,  testY ) <- readAndParse te
    let
        xss_all                              = toVecOfColumns trainX 
        xss_test                             = toVecOfColumns testX
        (xss_train, y_train, xss_val, y_val) = splitValidation 0.5 trainX trainY

        measureList = fromList $ getMeasure mcfg
        -- Create the fitness function for the training and test set 
        fitTrain    = evalTrain task measureList (fromShapes shapes domains) penalty xss_train y_train xss_val y_val 
        refit       = evalTrain task measureList (fromShapes shapes domains) penalty xss_all trainY xss_all trainY
        fitTest     = evalTest task measureList xss_test testY
        dim         = LA.cols trainX - 1

        (mutFun, rndTerm) = withMutation mcfg mcfgR dim            -- create the mutation function

        p0       = initialPop (getMaxTerms mcfg) nPop rndTerm fitTrain
        gens     = alg p0 mutFun fitTrain g 

    genReports output measureList gens nGens fitTest refit                      -- create the report


-- evaluate a lazy stream of infinity generations
runITEA, runRITEA, runFI2POP :: AlgRunner 
runITEA p0 mutFun fitTrain g = (p0 >>= itea mutFun fitTrain) `evalState` g 

runRITEA p0 mutFun fitTrain g = (p0 >>= ritea mutFun fitTrain) `evalState` g 

runFI2POP p0 mutFun fitTrain g = 
  let p0' = splitPop <$> p0
      p   = (p0' >>= fi2pop mutFun fitTrain) `evalState` g
  in  map fst p
