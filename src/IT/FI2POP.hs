module IT.FI2POP where

import IT  
import IT.Algorithms
import IT.Random
import IT.ITEA

import Control.Monad.Extra (iterateM)

import GHC.Conc (numCapabilities)

import Control.Monad.State
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Maybe
import System.Random
import Data.List (nub)

-- * FI2POP

-- | Creates a stream of generations the /i/-th 
-- element corresponds to the population of the /i/-th generation.
fi2pop :: Mutation -> Fitness -> (Population, Population) -> Rnd [(Population, Population)]
fi2pop f g (feas0, infeas0) = let n = length feas0 + length infeas0
                              in  iterateM (step2pop f g n) (feas0, infeas0)

splitPop :: Population -> (Population, Population)
splitPop pop = go pop [] []
  where
    go []     feas infeas = (feas, infeas)
    go (p:ps) feas infeas
      | _constr p == 0 = go ps (p:feas) infeas
      | otherwise      = go ps feas (p:infeas)
      
-- splitPop <$> initialPop


-- | Perform one iteration of FI2POP
step2pop :: Mutation -> Fitness -> Int -> (Population, Population) -> Rnd (Population, Population)
step2pop mutFun fitFun nPop (feas, infeas) = do
  childrenF <- parRndMap nPop (mutFun . _expr) fitFun feas
  childrenI <- parRndMap nPop (mutFun . _expr) fitFun infeas
  let (feas', infeas') = splitPop (childrenF ++ childrenI)
      nFeas            = max (length feas) (length feas')
      nInfeas          = max (length infeas) (length infeas')
  nextFeas <- if null feas'
                then tournament feas nFeas
                else tournament (feas ++ feas') (min nFeas nPop)
  nextInfeas <- if null infeas'
                  then tournament infeas nInfeas
                  else tournament (infeas ++ infeas') (min nInfeas nPop)
  return (nextFeas, nextInfeas)

