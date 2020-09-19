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

import Data.Char
import System.Environment

import RunCrossVal
import RunConfig

parse :: [String] -> IO  ()
parse (mode:cs)
  | map toLower mode == "config"   = parseConfigFile cs
  | map toLower mode == "crossval" = runCrossVal cs
parse _                            = putStrLn "Usage: ./itea {config/crossval} {config-file-name/dataset fold}"

main :: IO ()
main = getArgs >>= parse
