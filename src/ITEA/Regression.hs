{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
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

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V

import Control.Monad.State
import System.Random.SplitMix

-- | Support function for running ITEA
runITEAReg :: Datasets     -- training and test datasets
           -> MutationCfg  -- configuration of mutation operators
           -> Output       -- output to Screen | PartialLog filename | FullLog filename
           -> Int          -- population size
           -> Int          -- generations
           -> IO ()
runITEAReg (D tr te) mcfg output nPop nGens =
 do g <- initSMGen
    (trainX, trainY) <- parseFile <$> readFile tr
    (testX,  testY ) <- parseFile <$> readFile te
    let
        xss      = V.fromList $ LA.toColumns trainX
        xss'     = V.fromList $ LA.toColumns testX
        fitTrain = fitnessReg xss trainY    -- create the fitness function
        fitTest  = fitnessTest xss' testY         -- create the fitness for the test set
        dim      = LA.cols trainX

        (mutFun, rndTerm)   = withMutation mcfg dim            -- create the mutation function

        p0       = initialPop dim 4 nPop rndTerm fitTrain      -- initialize de population
        gens     = (p0 >>= itea mutFun fitTrain) `evalState` g -- evaluate a lazy stream of infinity generations
        best     = getBest nGens gens                          -- get the best solution of the first gens generations

    genReports output gens nGens fitTest                       -- create the report
