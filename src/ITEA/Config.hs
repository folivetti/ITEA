{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}

module ITEA.Config where

import System.Directory
import System.IO
import System.Clock

import IT
import IT.Algorithms
import IT.Mutation
import IT.Regression
import IT.Random

import qualified Numeric.LinearAlgebra as LA
import Control.Monad.State
import qualified MachineLearning as ML
import Data.List.Split (splitOn)
import Data.List (intersperse)

-- | Configuration Validation
class Monoid a => Valid a b | a -> b, b -> a where
  validateConfig :: a -> b

data Param a = None | Has a deriving Show

fromParam :: Param a -> a
fromParam (Has x) = x

instance Semigroup (Param a) where
  p <> None = p
  _ <> p    = p
instance Monoid (Param a) where
  mempty = None
  
-- | Mutation configuration
data Funcs = FLinear | FNonLinear | FTrig | FAll deriving (Read, Show)

data UncheckedMutationCfg = UMCfg { _expLim   :: (Param (Int, Int))
                                  , _termLim  :: (Param (Int, Int))
                                  , _nzExp    :: Param Int
                                  , _transFun :: (Param Funcs)
                                  }
data MutationCfg = MCfg (Int, Int) (Int, Int) Int Funcs deriving Show

instance Semigroup UncheckedMutationCfg where
  (UMCfg p1 p2 p3 p4) <> (UMCfg q1 q2 q3 q4) = UMCfg (p1<>q1) (p2<>q2) (p3<>q3) (p4<>q4)
instance Monoid UncheckedMutationCfg where
  mempty = UMCfg mempty mempty mempty mempty
  
exponents :: Int -> Int -> UncheckedMutationCfg
exponents x y = mempty { _expLim   = Has (x,y) }

termLimit :: Int -> Int -> UncheckedMutationCfg
termLimit   x y = mempty { _termLim  = Has (x,y) }

nonzeroExps :: Int -> UncheckedMutationCfg
nonzeroExps x = mempty { _nzExp = Has x }

transFunctions :: Funcs -> UncheckedMutationCfg
transFunctions  fs  = mempty { _transFun = Has fs }

instance Valid UncheckedMutationCfg MutationCfg where
  -- validateConfig :: UncheckedMutationCfg -> MutationCfg
  validateConfig (UMCfg None _ _ _) = error "No exponent limits set"
  validateConfig (UMCfg _ None _ _) = error "No expression size limits set"
  validateConfig (UMCfg _ _ None _) = error "No maximum non-zero exponents set"
  validateConfig (UMCfg _ _ _ None) = error "No transformation functions chosen"
  validateConfig c = MCfg (pexpLim c) (ptermLim c) (pnzExp c) (ptransFun c)
    where
      pexpLim   = fromParam . _expLim
      ptermLim  = fromParam . _termLim
      pnzExp    = fromParam . _nzExp
      ptransFun = fromParam . _transFun      

getMaxTerms (MCfg _ (_, maxTerms) _ _) = maxTerms

-- | Parse a numerical csv file into predictors and target variables
parseFile :: String -> (LA.Matrix Double, Vector)
parseFile css = ML.splitToXY . LA.fromLists $ map (map read) dat
  where
    dat = map (splitOn ",") $ lines css

-- | Creates the mutation function and also returns the random term generator (for initialization)
withMutation :: MutationCfg -> Int -> (Mutation Double, Rnd (Term Double))
withMutation (MCfg elim tlim nzExp transfun) dim = (mutFun dim elim tlim rndTerm rndTrans, rndTerm)
  where
    trans FLinear = regLinear
    trans FNonLinear = regNonLinear
    trans FTrig = regTrig
    trans FAll = regAll
    (minExp, maxExp) = elim
    rndInter = sampleInterMax dim nzExp minExp maxExp
    rndTrans = sampleTrans (trans transfun)
    rndTerm  = sampleTerm rndTrans rndInter

-- | Datasets configuration

data UncheckedDatasets = UD { _trainset :: Param String, _testset :: Param String }  deriving Show
data Datasets = D String String deriving Show

trainingset, testset :: String -> UncheckedDatasets
trainingset name = mempty { _trainset = Has name }
testset     name = mempty { _testset = Has name }

instance Semigroup UncheckedDatasets where
  (UD p1 p2) <> (UD q1 q2) = UD (p1<>q1) (p2<>q2)
instance Monoid UncheckedDatasets where
  mempty = UD mempty mempty

instance Valid UncheckedDatasets Datasets where
  validateConfig (UD None _) = error "No training data was set"
  validateConfig (UD _ None) = error "No test data was set"
  validateConfig (UD tr te) = D (fromParam tr) (fromParam te)
  
-- | Output configuration  
data Output = Screen | PartialLog String | FullLog String deriving Read

getBest :: Int  -> [Population Double RegStats] -> Solution Double RegStats
getBest n p     = minimum $ getAllBests n p
getAllBests n p = map minimum (take n p) 

data StatType = Best | Worst | Avg deriving Show

applyStat f n p = map (map f) $ take n p

getAll :: StatType -> (RegStats -> Double) -> Int -> [[RegStats]] -> [Double]
getAll Best f n p  = map minimum (applyStat f n p) 
getAll Worst f n p = map maximum (applyStat f n p)
getAll Avg   f n p = map mean    (applyStat f n p)
  where mean xs = sum xs / fromIntegral (length xs)

-- | gets all stats at once instead of going through the list multiple times
getAllStats :: Int -> [[RegStats]] -> [AggStats]
getAllStats n p = map myfold (take n p)
  where
    myfold :: [RegStats] -> AggStats
    myfold (p:pi) = let a0 = AS p p p
                        f p4 (AS p1 p2 p3) = AS (best p1 p4) (worst p2 p4) (avg p3 p4)
                        a1 = foldr f a0 pi
                        n  = fromIntegral $ length pi + 1
                     in getAvg a1 n
                     
    best  (RS x1 x2 x3 x4 x5) (RS y1 y2 y3 y4 _) = RS (min x1 y1) (min x2 y2) (min x3 y3) (max x4 y4) x5
    worst (RS x1 x2 x3 x4 x5) (RS y1 y2 y3 y4 _) = RS (max x1 y1) (max x2 y2) (max x3 y3) (min x4 y4) x5
    avg   (RS x1 x2 x3 x4 x5) (RS y1 y2 y3 y4 _) = RS (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) x5
    getAvg (AS a1 a2 (RS x1 x2 x3 x4 x5))  n = AS  a1 a2 (RS (x1/n) (x2/n) (x3/n) (x4/n) x5)

-- | Creates a file if it does not exist
createIfDoesNotExist fname = do
  isCreated <- doesFileExist fname
  h <- if   isCreated
       then openFile fname AppendMode
       else openFile fname WriteMode
  if isCreated then hPutStrLn h "" else hPutStrLn h headReport
  return h

-- | Generates the reports into the output
genReports :: Output -> [Population Double RegStats] -> Int -> (Solution Double RegStats -> RegStats) -> IO ()
genReports Screen pop n fitTest = do
  let best = getBest n pop
  putStrLn "Best expression applied to the training set:\n"
  print best
  putStrLn "Best expression applied to the test set:\n"
  print (fitTest best)
  
genReports (PartialLog dirname) pop n fitTest = do
  createDirectoryIfMissing True dirname
  let fname = dirname ++ "/stats.csv"
  
  hStats <- createIfDoesNotExist fname

  let best = getBest n pop
  
  t0 <- getTime Realtime
  print best
  t1 <- getTime Realtime

  let e = _expr best
    
  let bestTest = fitTest best
      stats = concat $ intersperse "," $ [dirname, show (sec t1 - sec t0)] ++ resultsToStr best bestTest      
  
  hPutStr hStats stats
  hClose hStats

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

data AggStats = AS { _best  :: RegStats
                   , _worst :: RegStats
                   , _avg   :: RegStats
                   }

data Metric = RMSE | MAE | NMSE | R2 deriving Show

genEvoStream [x] hs = do
  let zs = map (show . ($ x)) [_rmse, _mae, _nmse, _r2]
  zipWithM_ hPutStr hs zs

genEvoStream (x:xs) hs = do
  let zs = map ((++",") . show . ($ x)) [_rmse, _mae, _nmse, _r2]
  zipWithM_ hPutStr hs zs
  genEvoStream xs hs

genEvoReport :: [AggStats] -> String -> IO ()
genEvoReport stats dirname = do
  let names     = fmap (dirname++) ["Rmse", "Mae", "Nmse", "R2"]
      nameBest  = fmap (++"Best.csv") names
      nameWorst = fmap (++"Worst.csv") names
      nameAvg   = fmap (++"Avg.csv") names
  hsBest   <- mapM (\n -> openFile n AppendMode) nameBest
  hsWorst  <- mapM (\n -> openFile n AppendMode) nameWorst
  hsAvg    <- mapM (\n -> openFile n AppendMode) nameAvg
  genEvoStream (fmap _best  stats) hsBest
  genEvoStream (fmap _worst stats) hsWorst
  genEvoStream (fmap _avg   stats) hsAvg
  mapM_ hClose hsBest
  mapM_ hClose hsWorst
  mapM_ hClose hsAvg

resultsToStr :: Solution Double RegStats -> RegStats -> [String]
resultsToStr train stest = (map show statlist) ++ [show (_expr train)]
  where 
    strain = _stat train
    statlist = [_rmse strain, _rmse stest, _mae strain, _mae stest, _nmse strain, _nmse stest, _r2 strain, _r2 stest]

headReport = "name,time,RMSE_train,RMSE_test,MAE_train,MAE_test,NMSE_train,NMSE_test,r2_train,r2_test,expr"
