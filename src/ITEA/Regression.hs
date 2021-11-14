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
import IT.ITEA
import IT.Algorithms
import IT.FI2POP
import IT.Regression
import IT.Shape

import ITEA.Config
import ITEA.Report

import Data.List.NonEmpty hiding (map, zipWith)

import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.Morpheus.MatrixReduce as MTX
import qualified Data.Vector as V
import Numeric.Interval ((...), Interval)

import Control.Monad.State
import System.Random

type AlgRunner = Rnd Population -> Mutation -> Fitness -> StdGen -> [Population]

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
  | nRows <= 50 = (toVecOfColumns xss, ys, toVecOfColumns xss, ys)
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
    -> Maybe Int          -- random seed
    -> Datasets     -- training and test datasets
    -> MutationCfg  -- configuration of mutation operators
    -> Output       -- output to Screen | PartialLog filename | FullLog filename
    -> Int          -- population size
    -> Int          -- generations
    -> Task
    -> Penalty
    -> [Shape]
    -> Domains
    -> IO ()
run alg seed (D tr te) mcfg output nPop nGens task penalty shapes domains =
 do g <- case seed of 
           Nothing -> newStdGen
           Just s  -> return $ mkStdGen s
    (trainX, trainY) <- readAndParse tr
    (testX,  testY ) <- readAndParse te
    let
        xss_all                              = toVecOfColumns trainX 
        xss_test                             = toVecOfColumns testX
        (xss_train, y_train, xss_val, y_val) = splitValidation 0.3 trainX trainY

        minX = Prelude.tail $ LA.toList $ MTX.columnPredicate min trainX
        maxX = Prelude.tail $ LA.toList $ MTX.columnPredicate max trainX
        domains' :: [Interval Double]
        domains' = case domains of
                     Nothing -> zipWith (...) minX maxX
                     Just ds -> map (uncurry (...)) ds

        measureList = fromList $ getMeasure mcfg
        -- Create the fitness function for the training and test set 
        fitTrain    = evalTrain task measureList (fromShapes shapes domains) penalty xss_train y_train xss_val y_val domains'
        refit       = evalTrain task measureList (fromShapes shapes domains) penalty xss_all trainY xss_all trainY domains'
        fitTest     = evalTest task measureList xss_test testY
        dim         = LA.cols trainX - 1

        (mutFun, rndTerm) = withMutation mcfg dim            -- create the mutation function

        p0       = initialPop (getMaxTerms mcfg) nPop rndTerm fitTrain
        gens     = alg p0 mutFun fitTrain g 

    genReports output measureList gens nGens fitTest refit                      -- create the report


-- evaluate a lazy stream of infinity generations
runITEA, runFI2POP :: AlgRunner 
runITEA p0 mutFun fitTrain g = (p0 >>= itea mutFun fitTrain) `evalState` g 

runFI2POP p0 mutFun fitTrain g = 
  let p0' = splitPop <$> p0
      p   = (p0' >>= fi2pop mutFun fitTrain) `evalState` g
  in  map fst p
