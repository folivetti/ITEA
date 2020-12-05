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
import IT.Regression
import ITEA.Config
import ITEA.Report

import Data.List.NonEmpty

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V

import Control.Monad.State
import System.Random

-- | Support function for running ITEA
runITEAReg :: Datasets     -- training and test datasets
           -> MutationCfg  -- configuration of mutation operators
           -> Output       -- output to Screen | PartialLog filename | FullLog filename
           -> Int          -- population size
           -> Int          -- generations
           -> IO ()
runITEAReg (D tr te) mcfg output nPop nGens =
 do g <- newStdGen
    (trainX, trainY) <- parseFile <$> readFile tr
    (testX,  testY ) <- parseFile <$> readFile te
    let
        xss         = V.fromList $ LA.toColumns trainX
        xss'        = V.fromList $ LA.toColumns testX
        measureList = fromList $ getMeasure mcfg -- [_rmse, _mae, _nmse, _r2]
        fitTrain    = evalTrain Regression measureList xss trainY        -- create the fitness function
        fitTest     = evalTest Regression measureList xss' testY         -- create the fitness for the test set
        dim         = LA.cols trainX

        (mutFun, rndTerm)   = withMutation mcfg dim            -- create the mutation function

        p0       = initialPop 4 nPop rndTerm fitTrain      -- initialize de population
        gens     = (p0 >>= itea mutFun fitTrain) `evalState` g -- evaluate a lazy stream of infinity generations

    genReports output measureList gens nGens fitTest                       -- create the report
    
runITEAClass :: Datasets     -- training and test datasets
           -> MutationCfg  -- configuration of mutation operators
           -> Output       -- output to Screen | PartialLog filename | FullLog filename
           -> Int          -- population size
           -> Int          -- generations
           -> IO ()
runITEAClass (D tr te) mcfg output nPop nGens =
 do g <- newStdGen
    (trainX, trainY) <- parseFile <$> readFile tr
    (testX,  testY ) <- parseFile <$> readFile te
    let
        xss         = V.fromList $ LA.toColumns trainX
        xss'        = V.fromList $ LA.toColumns testX
        measureList = fromList $ getMeasure mcfg -- [_rmse, _mae, _nmse, _r2]
        fitTrain    = evalTrain Classification measureList xss trainY        -- create the fitness function
        fitTest     = evalTest Classification measureList xss' testY         -- create the fitness for the test set
        dim         = LA.cols trainX

        (mutFun, rndTerm)   = withMutation mcfg dim            -- create the mutation function

        p0       = initialPop 4 nPop rndTerm fitTrain      -- initialize de population
        gens     = (p0 >>= itea mutFun fitTrain) `evalState` g -- evaluate a lazy stream of infinity generations

    genReports output measureList gens nGens fitTest                       -- create the report    
    
runITEAClassMult :: Datasets     -- training and test datasets
           -> MutationCfg  -- configuration of mutation operators
           -> Output       -- output to Screen | PartialLog filename | FullLog filename
           -> Int          -- population size
           -> Int          -- generations
           -> IO ()
runITEAClassMult (D tr te) mcfg output nPop nGens =
 do g <- newStdGen
    (trainX, trainY) <- parseFile <$> readFile tr
    (testX,  testY ) <- parseFile <$> readFile te
    let
        xss         = V.fromList $ LA.toColumns trainX
        xss'        = V.fromList $ LA.toColumns testX
        measureList = fromList $ getMeasure mcfg -- [_rmse, _mae, _nmse, _r2]
        fitTrain    = evalTrain ClassMult measureList xss trainY        -- create the fitness function
        fitTest     = evalTest ClassMult measureList xss' testY         -- create the fitness for the test set
        dim         = LA.cols trainX

        (mutFun, rndTerm)   = withMutation mcfg dim            -- create the mutation function

        p0       = initialPop 4 nPop rndTerm fitTrain      -- initialize de population
        gens     = (p0 >>= itea mutFun fitTrain) `evalState` g -- evaluate a lazy stream of infinity generations

    genReports output measureList gens nGens fitTest                       -- create the report        
