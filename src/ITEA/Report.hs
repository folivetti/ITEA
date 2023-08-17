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

import IT
import IT.Algorithms
import IT.Metrics

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Numeric.LinearAlgebra as LA
import Data.Maybe
import Data.List (intercalate, foldl')
import Control.Monad

-- | Output configuration  
data Output = Screen | PartialLog String | FullLog String deriving Read

-- | Get best solution from all generations
getBest :: Int  -> [Population] -> Solution
getBest n ps     = minimum $ getAllBests n ps

getBestMaybe :: Int -> [Population] -> Maybe Solution
getBestMaybe n ps = case getAllBests n ps of
                       [] -> Nothing
                       xs -> Just (minimum xs)

getAllBests :: Int -> [Population] -> [Solution]
getAllBests n ps = concatMap minimum' $ take n ps
  where
    minimum' [] = []
    minimum' xs = [minimum xs]

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
genReports :: Output -> NonEmpty Measure -> [Population] -> Int -> (Solution -> Maybe [Double]) -> (Expr -> Maybe Solution) -> IO ()
genReports Screen _ pop n fitTest refit = do
  let best  = getBest n pop
      best' = case refit (_expr best) of
                Nothing -> best
                Just x  -> x
  putStrLn "Best expression applied to the training set:\n"
  print best'
  putStrLn "Best expression applied to the test set:\n"
  print (fitTest best')

genReports (PartialLog dirname) measures pop n fitTest refit = do
  createDirectoryIfMissing True dirname
  let
    fname      = dirname ++ "/stats.csv"
    fnameExpr  = dirname ++ "/exprs.csv"
    mNames     = NE.map _name measures
    trainNames = NE.toList $ NE.map (++"_train") mNames
    testNames  = NE.toList $ NE.map (++"_test") mNames
    headReport = intercalate "," (["name", "time", "length"] ++ interleave trainNames testNames)
    headExpr   = intercalate "," ["expr", "weights", "python"]
    best'      = getBest n pop
    best       = case refit (_expr best') of
                   Nothing -> best'
                   Just x  -> x

  hStats     <- createIfDoesNotExist headReport fname
  hStatsExpr <- createIfDoesNotExist headExpr fnameExpr

  t0 <- getTime Realtime
  print best -- force evaluation. Don't be lazy.
  t1 <- getTime Realtime

  let
    totTime         = show $ sec t1 - sec t0
    nFit            = length (_fit best)
    bestTest        = fromMaybe (replicate nFit (1/0)) $ fitTest best
    measuresResults = map show $ interleave (_fit best) bestTest
    exprWithWeight  = "\"" ++ toExprStr expr ws ++ "\",\"" ++ show ws ++ "\",\"" ++ toPython expr ws ++ "\""
    ws              = LA.toList . head $ _weights best
    expr            = _expr best

    stats           = intercalate "," $ [dirname, totTime, show (_len best)] ++ measuresResults

  hPutStr hStats stats
  hPutStr hStatsExpr exprWithWeight
  hClose hStats
  hClose hStatsExpr

-- FullLog is the same as PartialLog plus the best, worst, avg fit for every generation.
--
-- TODO: this code is ugly, but it's low priority to fix it.
genReports (FullLog dirname) measures pop n fitTest refit =
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

    genReports (PartialLog dirname) measures pop n fitTest refit

-- | Opens the first file available in the format "name.{i}.csv"
-- where 'i' follows a sequence from 0 onward.
openNext :: String -> IO Handle
openNext fname = go [fname ++ "." ++ show n ++ ".csv" | n <- [0 :: Int ..]]
  where
    -- this is a partial function applied to an infinite list
    -- so, what harm can it do?
    go []       = error "end of inifinity stream"
    go (fn:fns) = do b <- doesFileExist fn
                     if b
                        then go fns
                        else openFile fn WriteMode

postAgg :: [Double] -> [Double]
postAgg [best, worst, tot, count] = [best, worst, tot/count]
postAgg _ = error "wrong parameters count"

aggregate :: [Double] -> Double -> [Double]
aggregate [] train = [train,train,train,1]
aggregate [best, worst, tot, count] train = [min best train, max worst train, tot+train, count+1]
aggregate _ _ = error "wrong parameters count in aggregate"

-- | Write a sequence to a sequence of opened files
toFile :: [Handle] -> [Double] -> IO ()
toFile hs ps = zipWithM_ hPutStrLn hs
             $ map show ps
