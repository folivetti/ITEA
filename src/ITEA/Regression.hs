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

import IT.ITEA
import IT.FI2POP
import IT.Regression
import IT.Shape

import ITEA.Config
import ITEA.Report

import Data.List.NonEmpty hiding (map)

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V
import Numeric.Interval

import Control.Monad.State
import System.Random



-- | Support function for running ITEA
runITEA :: Datasets     -- training and test datasets
        -> MutationCfg  -- configuration of mutation operators
        -> Output       -- output to Screen | PartialLog filename | FullLog filename
        -> Int          -- population size
        -> Int          -- generations
        -> Task
        -> Penalty
        -> [Shape]
        -> Domains
        -> IO ()
runITEA (D tr te) mcfg output nPop nGens task penalty shapes domains =
 do g <- newStdGen
    (trainX, trainY) <- parseFile <$> readFile tr
    (testX,  testY ) <- parseFile <$> readFile te
    let
        xss         = V.fromList $ LA.toColumns trainX
        xss'        = V.fromList $ LA.toColumns testX
        measureList = fromList $ getMeasure mcfg
        fitTrain    = evalTrain task measureList (fromShapes shapes domains) penalty xss trainY        -- create the fitness function
        fitTest     = evalTest task measureList xss' testY         -- create the fitness for the test set
        dim         = LA.cols trainX

        (mutFun, rndTerm)   = withMutation mcfg dim            -- create the mutation function

        p0       = initialPop 4 nPop rndTerm fitTrain      -- initialize de population
        gens     = (p0 >>= itea mutFun fitTrain) `evalState` g -- evaluate a lazy stream of infinity generations

    genReports output measureList gens nGens fitTest                       -- create the report
    
runFI2POP :: Datasets     -- training and test datasets
          -> MutationCfg  -- configuration of mutation operators
          -> Output       -- output to Screen | PartialLog filename | FullLog filename
          -> Int          -- population size
          -> Int          -- generations
          -> Task
          -> Penalty
          -> [Shape]
          -> Domains
          -> IO ()
runFI2POP (D tr te) mcfg output nPop nGens task penalty shapes domains =
 do g <- newStdGen
    (trainX, trainY) <- parseFile <$> readFile tr
    (testX,  testY ) <- parseFile <$> readFile te
    let
        xss         = V.fromList $ LA.toColumns trainX
        xss'        = V.fromList $ LA.toColumns testX
        measureList = fromList $ getMeasure mcfg
        fitTrain    = evalTrain task measureList (fromShapes shapes domains) penalty xss trainY        -- create the fitness function
        fitTest     = evalTest task measureList xss' testY         -- create the fitness for the test set
        dim         = LA.cols trainX

        (mutFun, rndTerm)   = withMutation mcfg dim            -- create the mutation function

        p0       = splitPop <$> initialPop 4 nPop rndTerm fitTrain      -- initialize de population
        gens     = map fst $ (p0 >>= fi2pop mutFun fitTrain) `evalState` g -- evaluate a lazy stream of infinity generations

    genReports output measureList gens nGens fitTest                       -- create the report    
