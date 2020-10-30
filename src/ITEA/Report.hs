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
import Data.List (intercalate, transpose, foldl1', foldl')
import Control.Monad
import Control.DeepSeq

-- | Output configuration  
data Output = Screen | PartialLog String | FullLog String deriving Read

-- | Get best solution from all generations
getBest :: Int  -> [Population Double] -> Solution Double
getBest n ps     = minimum $ getAllBests n ps

getAllBests :: Int -> [Population Double] -> [Solution Double]
getAllBests n ps = map minimum $ take n ps

-- | Creates a file if it does not exist
createIfDoesNotExist :: String -> FilePath -> IO Handle
createIfDoesNotExist headReport fname = do
  isCreated <- doesFileExist fname
  h <- if   isCreated
       then openFile fname AppendMode
       else openFile fname WriteMode
  if isCreated then hPutStrLn h "" else hPutStrLn h headReport
  return h

interleave :: [a] -> [a] -> [a]
interleave xs' ys' = getLeft xs' ys' []
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
    nFit            = length (_fit best)
    bestTest        = fromMaybe (replicate nFit (1/0)) $ fitTest best
    measuresResults = map show $ interleave (_fit best) bestTest
    stats           = intercalate "," $ [dirname, totTime] ++ measuresResults ++ [show $ _expr best]

  hPutStr hStats stats
  hClose hStats

-- map _fit pop :: Generations (Population [Double])
-- map getBest :: Generations [Double]
-- transpose :: [Generations Double] -- cada elemento é uma metrica 
genReports (FullLog dirname) measures pop n fitTest =
  do
    let
        pop'      = take n pop

        statsTrain = map (postAgg . foldl' aggregate [] . map (head._fit)) pop'
        statsTest  = map (postAgg . foldl' aggregate [] . map getTest) pop'

        getTest      = replaceWithNan . fitTest

        replaceWithNan Nothing      = 1/0
        replaceWithNan (Just [])    = 1/0
        replaceWithNan (Just (x:_)) = x

        statsNamesTrain = ["TrainBest", "TrainWorst", "TrainAvg"]
        statsNamesTest  = ["TestBest", "TestWorse", "TestAvg"]
        fulldirname     = dirname ++ "/FullLog"
        fnamesTrain     = map (\s -> fulldirname++"/"++s) statsNamesTrain
        fnamesTest      = map (\s -> fulldirname++"/"++s) statsNamesTest

    createDirectoryIfMissing True fulldirname
    hs <- sequence $ openNext <$> fnamesTrain ++ fnamesTest
    mapM_ (toFile hs) $ zipWith (++) statsTrain statsTest
    mapM_ hClose hs

    genReports (PartialLog dirname) measures pop n fitTest

openNext :: String -> IO Handle
openNext fname = go [fname ++ "." ++ show n ++ ".csv" | n <- [0..]]
  where
    go (fn:fns) = do b <- doesFileExist fn
                     if b
                        then go fns
                        else openFile fn WriteMode

postAgg :: [Double] -> [Double]
postAgg [best, worst, tot, count] = [best, worst, tot/count]

aggregate :: [Double] -> Double -> [Double]
aggregate [] train = [train,train,train,1]
aggregate [best, worst, tot, count] train = [min best train, max worst train, tot+train, count+1]
aggregate _ _ = error "anti pattern in aggregate"

toFile :: [Handle] -> [Double] -> IO ()
toFile hs ps = zipWithM_ hPutStrLn hs
             $ map show ps
