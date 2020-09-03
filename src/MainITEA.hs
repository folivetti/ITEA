{-|
Module      : ITEA
Description : Main program to run ITEA with a configuration file.
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Main program to run ITEA with a configuration file.
-}
module Main where

import System.Environment

import Data.ConfigFile
import Data.Either.Utils

import ITEA.Regression
import ITEA.Config

-- | get a given setting from a config file
getSetting cp cat x = forceEither $ get cp cat x

-- | Read the config file and run the algorithm
readConfig :: String -> IO ()
readConfig fname = do
  cp <- return . forceEither =<< readfile emptyCP fname 
  let 
    (expmin, expmax)   = getSetting cp "Mutation"  "exponents"
    (termmin, termmax) = getSetting cp "Mutation"  "termlimit"
    nzExps             = getSetting cp "Mutation"  "nonzeroexps"
    tfuncs             = getSetting cp "Mutation"  "transfunctions"
    trainname          = getSetting cp "Dataset"   "train"
    testname           = getSetting cp "Dataset"   "test"
    nPop               = getSetting cp "Algorithm" "npop"
    nGens              = getSetting cp "Algorithm" "ngens"
    log                = getSetting cp "Algorithm" "log"

    -- validate the configurations
    mutCfg =  validateConfig
           $  exponents expmin expmax
           <> termLimit termmin termmax
           <> nonzeroExps nzExps
           <> transFunctions tfuncs

    datasetCfg =  validateConfig
               $  trainingset trainname
               <> testset testname

  -- run ITEA with given configuration
  runITEAReg datasetCfg mutCfg log nPop nGens

parse :: [String] -> IO  ()
parse [fname] = readConfig fname
parse _       = putStrLn "Usage: ./itea config-file-name"

main :: IO ()
main = getArgs  >>= parse
