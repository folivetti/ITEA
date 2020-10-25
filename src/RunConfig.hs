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

-- | Read the option 'x' located at section 'cat' from configuration 'cp'.
getSetting :: Get_C a => ConfigParser -> SectionSpec -> OptionSpec -> a
getSetting cp cat x = forceEither $ get cp cat x

-- | Read the config file and run the algorithm.
runWithConfig :: String -> IO ()
runWithConfig fname = do
  cp <- forceEither <$> readfile emptyCP fname
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

  -- run ITEA with the given configuration
  runITEAReg datasetCfg mutCfg log nPop nGens

-- | Parse the filename from the system arguments.
parseConfigFile :: [String] -> IO ()
parseConfigFile [fname] = do exist <- doesFileExist fname
                             if exist
                              then runWithConfig fname
                              else putStrLn "Config file does not exist."
parseConfigFile _       = putStrLn "Usage: ./itea config config-file-name"
