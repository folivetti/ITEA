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
module RunConfig where

import System.Directory

import Data.ConfigFile
import Data.Either.Utils

import ITEA.Regression
import ITEA.Config
import IT.Regression

-- | Algorithm Choice 
--
-- ITEA is the mutation-based Symbolic Regression for IT representation
-- FI2POP splits the population as feasible and infeasible to deal with constraints
data Alg = ITEA | FI2POP deriving (Show, Read)

-- | Read the option 'x' located at section 'cat' from configuration 'cp'.
getSetting :: Get_C a => ConfigParser -> SectionSpec -> OptionSpec -> a
getSetting cp cat x = forceEither $ get cp cat x

-- | get a setting value if it exists in the config file, or return a default value 
getWithDefault :: Get_C a => a -> ConfigParser -> SectionSpec -> OptionSpec -> a
getWithDefault def cp cat x = 
  case get cp cat x of
    Right y -> y      
    Left  (NoOption _, _) -> def
    Left  (NoSection _, _) -> def
    Left (e,_) -> error (show e)

-- | Read the config file and run the algorithm.
runWithConfig :: String -> IO ()
runWithConfig fname = do
  cp <- forceEither <$> readfile emptyCP fname
  let
    (expmin, expmax)   = getSetting cp "Mutation"  "exponents"
    (termmin, termmax) = getSetting cp "Mutation"  "termlimit"
    nzExps             = getSetting cp "Mutation"  "nonzeroexps"
    tfuncs             = getSetting cp "Mutation"  "transfunctions"
    perf_mes           = getSetting cp "Mutation"  "measures"
    
    trainname          = getSetting cp "IO"   "train"
    testname           = getSetting cp "IO"   "test"
    task               = getSetting cp "IO"   "task"
    logg               = getSetting cp "IO"   "log"
    
    nPop               = getSetting cp "Algorithm"   "npop"
    nGens              = getSetting cp "Algorithm"   "ngens"
    alg                = getSetting cp "Algorithm"   "algorithm"
    seed               = getWithDefault Nothing cp "Algorithm" "seed"
    
    penalty            = getWithDefault NoPenalty cp "Constraints" "penalty"
    shapes             = getWithDefault [] cp "Constraints" "shapes"
    domains            = getWithDefault Nothing cp "Constraints" "domains"
    
    -- validate the configurations
    mutCfg =  validateConfig
           $  exponents expmin expmax
           <> termLimit termmin termmax
           <> nonzeroExps nzExps
           <> transFunctions tfuncs
           <> measures perf_mes 

    datasetCfg =  validateConfig
               $  trainingset trainname
               <> testset testname

  -- run ITEA with the given configuration
  case alg of
    ITEA   -> run runITEA seed datasetCfg mutCfg logg nPop nGens task penalty shapes domains
    FI2POP -> run runFI2POP seed datasetCfg mutCfg logg nPop nGens task penalty shapes domains

-- | Parse the filename from the system arguments.
parseConfigFile :: [String] -> IO ()
parseConfigFile [fname] = do exist <- doesFileExist fname
                             if exist
                              then runWithConfig fname
                              else putStrLn "Config file does not exist."
parseConfigFile _       = putStrLn "Usage: ./itea config config-file-name"
