{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : Example.Regression
Description : Example of usage for Symbolic Regression
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Configuration parsing and report generation.
-}
module ITEA.Report where

import System.Directory
import System.IO
import System.Clock

import IT.Algorithms
import IT.Metrics

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.List (intercalate)

-- | Output configuration  
data Output = Screen | PartialLog String | FullLog String deriving Read

-- | Get best solution from all generations
getBest :: Int  -> [Population Double] -> Solution Double
getBest n ps     = minimum $ getAllBests n ps
getAllBests n ps = map minimum $ take n ps

data AggStats = AS { _best  :: NonEmpty Double
                   , _worst :: NonEmpty Double
                   , _avg   :: NonEmpty Double
                   }

-- | gets all stats at once instead of going through the list multiple times
getAllStats :: Int -> [[NonEmpty Double]] -> [AggStats]
getAllStats n p = map myfold $ take n p
  where
    combineAS (AS x y z) (AS a b c) = AS (best x a) (worst y b) (avg z c)

    nPop = fromIntegral $ length $ head p

    myfold :: [NonEmpty Double] -> AggStats
    myfold ps = getAvg
              $ foldr1 combineAS
              $ map (\p -> AS p p p) ps

    best  = NE.zipWith min
    worst = NE.zipWith max
    avg   = NE.zipWith (+)

    getAvg (AS a1 a2 a3) = AS a1 a2 (NE.map (/nPop) a3)

-- | Creates a file if it does not exist
createIfDoesNotExist headReport fname = do
  isCreated <- doesFileExist fname
  h <- if   isCreated
       then openFile fname AppendMode
       else openFile fname WriteMode
  if isCreated then hPutStrLn h "" else hPutStrLn h headReport
  return h

interleave :: [a] -> [a] -> [a]
interleave xs ys = getLeft xs ys []
  where
    getLeft [] ys zs      = zs ++ ys
    getLeft (x:xs) ys zs  = getRight xs ys (x:zs)
    getRight xs [] zs     = zs ++ xs
    getRight xs (y:ys) zs = getLeft xs ys (y:zs)


-- | Generates the reports into the output
genReports :: Output -> NonEmpty Measure -> [Population Double] -> Int -> (Solution Double -> Maybe [Double]) -> IO ()
genReports Screen _ pop n fitTest = do
  let best = getBest n pop
  putStrLn "Best expression applied to the training set:\n"
  print best
  putStrLn "Best expression applied to the test set:\n"
  print (fitTest best)

genReports (PartialLog dirname) measures pop n fitTest = do
  createDirectoryIfMissing True dirname
  let
    fname      = dirname ++ "/stats.csv"
    mNames     = NE.map _name measures
    trainNames = NE.toList $ NE.map (++"_train") mNames
    testNames  = NE.toList $ NE.map (++"_test") mNames
    headReport = intercalate "," (["name", "time"] ++ interleave trainNames testNames ++ ["expr"])
    best       = getBest n pop 

  hStats <- createIfDoesNotExist headReport fname

  t0 <- getTime Realtime
  print best -- force evaluation. Don't be lazy.
  t1 <- getTime Realtime

  let 
    totTime         = show $ sec t1 - sec t0
    n               = length (_fit best)
    bestTest        = fromMaybe (replicate n (1/0)) $ fitTest best
    measuresResults = map show $ interleave (_fit best) bestTest
    stats           = intercalate "," $ [dirname, totTime] ++ measuresResults ++ [show $ _expr best]

  hPutStr hStats stats
  hClose hStats

{-
genReports (FullLog dirname) pop n fitTest = do
  createDirectoryIfMissing True dirname
  let fname = dirname ++ "/stats.csv"

  hStats <- createIfDoesNotExist fname

  let best = getBest n pop

  t0 <- getTime Realtime
  print best
  t1 <- getTime Realtime

  let bestTest = fitTest best
      stats = concat $ intersperse "," $ [dirname, show (sec t1 - sec t0)] ++ resultsToStr best bestTest

  hPutStr hStats stats
  hClose hStats

  let statTrain = map (map _stat) pop
      statTest  = map (map fitTest) pop
      evoTrain  = getAllStats n statTrain
      evoTest   = getAllStats n statTest

  genEvoReport evoTrain (dirname ++ "/train")
  genEvoReport evoTest  (dirname ++ "/test")

genEvoStream [x] hs = do
  let zs = map (show . ($ x)) [_rmse, _mae, _nmse, _r2]
  zipWithM_ hPutStr hs zs

genEvoStream (x:xs) hs = do
  let zs = map ((++",") . show . ($ x)) [_rmse, _mae, _nmse, _r2]
  zipWithM_ hPutStr hs zs
  genEvoStream xs hs

-- | Generates evolution report 
genEvoReport :: [AggStats] -> String -> IO ()
genEvoReport stats dirname = do
  let names     = fmap (dirname++) ["Rmse", "Mae", "Nmse", "R2"]
      nameBest  = fmap (++"Best.csv") names
      nameWorst = fmap (++"Worst.csv") names
      nameAvg   = fmap (++"Avg.csv") names
  hsBest   <- mapM (`openFile` AppendMode) nameBest
  hsWorst  <- mapM (`openFile` AppendMode) nameWorst
  hsAvg    <- mapM (`openFile` AppendMode) nameAvg
  genEvoStream (fmap _best  stats) hsBest
  genEvoStream (fmap _worst stats) hsWorst
  genEvoStream (fmap _avg   stats) hsAvg
  mapM_ hClose hsBest
  mapM_ hClose hsWorst
  mapM_ hClose hsAvg
-}
