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
import IT.Eval

import ITEA.Config
import ITEA.Report

import Data.List.NonEmpty hiding (map)

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V

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

        nRows       = LA.rows trainX
        nRowsTrain  = round (fromIntegral nRows * 0.5 :: Double)
        nRowsVal    = nRows - nRowsTrain
        xss_train   = V.fromList $ LA.toColumns $ trainX LA.?? (LA.Take nRowsTrain, LA.All)
        xss_val     = V.fromList $ LA.toColumns $ trainX LA.?? (LA.Drop nRowsTrain, LA.All)
        y_train     = LA.subVector 0 nRowsTrain trainY
        y_val       = LA.subVector nRowsTrain nRowsVal trainY

        measureList = fromList $ getMeasure mcfg
        fitTrain    = evalTrain task measureList (fromShapes shapes domains) penalty xss_train y_train xss_val y_val        -- create the fitness function
        fitTest     = evalTest task measureList xss' testY         -- create the fitness for the test set
        refit       = evalTrain task measureList (fromShapes shapes domains) penalty xss trainY xss trainY
        cleaner     = cleanExpr xss_train
        
        dim         = LA.cols trainX

        (mutFun, rndTerm)   = withMutation mcfg dim            -- create the mutation function

        p0       = if nRows <= 100
                      then initialPop (getMaxTerms mcfg) nPop rndTerm refit cleaner      -- initialize the population
                      else initialPop (getMaxTerms mcfg) nPop rndTerm fitTrain cleaner      
        gens     = (p0 >>= itea mutFun fitTrain cleaner) `evalState` g -- evaluate a lazy stream of infinity generations

    genReports output measureList gens nGens fitTest refit                      -- create the report
    
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
        
        nRows       = LA.rows trainX
        -- TODO: add an option to create a validation set, for now it is disabled
        nRowsTrain  = round (fromIntegral nRows * 0.5 :: Double)
        nRowsVal    = nRows - nRowsTrain
        xss_train   = V.fromList $ LA.toColumns $ trainX LA.?? (LA.Take nRowsTrain, LA.All)
        xss_val     = V.fromList $ LA.toColumns $ trainX LA.?? (LA.Drop nRowsTrain, LA.All)
        y_train     = LA.subVector 0 nRowsTrain trainY
        y_val       = LA.subVector nRowsTrain nRowsVal trainY

        measureList = fromList $ getMeasure mcfg
        fitTrain    = evalTrain task measureList (fromShapes shapes domains) penalty xss_train y_train xss_val y_val        -- create the fitness function
        fitTest     = evalTest task measureList xss' testY         -- create the fitness for the test set
        refit       = evalTrain task measureList (fromShapes shapes domains) penalty xss trainY xss trainY
        cleaner     = cleanExpr xss_train
        
        dim         = LA.cols trainX

        (mutFun, rndTerm)   = withMutation mcfg dim            -- create the mutation function

        p0       = splitPop <$> initialPop (getMaxTerms mcfg) nPop rndTerm fitTrain cleaner     -- initialize de population
        p        = (p0 >>= fi2pop mutFun fitTrain) `evalState` g -- evaluate a lazy stream of infinity generations
        gens     = map fst p

    genReports output measureList gens nGens fitTest refit                      -- create the report    

