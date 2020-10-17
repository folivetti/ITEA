{-|
Module      : IT.ITEA
Description : Interaction-Transformation Evolutionary Algorithm
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Generic implementation of Interaction-Transformation Evolutionary Algorithm
for any instance of IT expression.

To run itea you just need to call 'itea mutFun pop0', 
where 'mutFun' is a mutation function of the type 'Mutation a b',
and 'pop0' is the initial 'Population a b' of solutions.
This function will result in an infinite list of populations, with
the /i/-th element being the population of the /i/-th generation.

This library also provides some generic mutation function builders.
-}
module IT.ITEA where

import IT -- (itea, addTerm, dropTerm)
import IT.Algorithms
import IT.Random

import Control.Monad.Extra (iterateM)

import GHC.Conc (numCapabilities)

import Control.Monad.State
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Maybe
import System.Random.SplitMix
-- ---------------------------------------------------------------------------

-- * ITEA

parRandom :: Int -> (a -> Rnd b) -> (b -> Maybe c) -> [a] -> Rnd [c]
parRandom n rndf f pop = state g
  where
    g seed = let (s1:ss) = genseeds seed
                 rndpop  = zip ss pop
                 h (s,p) = f $ evalState (rndf p) s
             in  (parMaybeMap n h rndpop, s1)

genseeds s = s1 : genseeds s2
  where
    (s1,s2) = splitSMGen s 

parMaybeMap n f pop = catMaybes res
  where res = map f pop `using` parListChunk n rpar

-- | Creates a stream of generations the /i/-th 
-- element corresponds to the population of the /i/-th generation.
itea :: (NFData a, NFData b) => Mutation a -> Fitness a b -> Population a b -> Rnd [Population a b]
itea f g p0 = let n = length p0
              in  iterateM (step f g n) p0

-- | Generate an Initial Population at Random
initialPop :: Int                -- ^ dimension
           -> Int                -- ^ maxTerms
           -> Int                -- ^ nPop
           -> Rnd (Term a)       -- ^ random term generator
           -> Fitness a b        -- ^ fitness function
           -> Rnd (Population a b)
initialPop dim maxTerms nPop rndTerm fit = parMaybeMap n fit <$> initialPop' dim maxTerms nPop
  where
    rndExpr = sampleExpr rndTerm
    n       = nPop `div` (2*numCapabilities)

    -- return a random list of random expressions
    initialPop' dim maxTerms 0    = return []
    initialPop' dim maxTerms nPop = do n  <- sampleRng 1 maxTerms
                                       e  <- rndExpr n
                                       es <- initialPop' dim maxTerms (nPop-1)
                                       return $ uniqueTerms e : es


-- | Tournament Selection
--
-- given the concatenation of the previous population
-- and the mutated children, it will return a sampled
-- selection of these combined population with
-- the same size as the original population.
--
tournament :: Population a b -> Int -> Rnd (Population a b)
tournament p 0 = return []
tournament p n = do pi <- chooseOne p
                    p' <- tournament p (n-1)
                    return $ pi:p'
  where
    chooseOne :: Population a b -> Rnd (Solution a b)
    chooseOne p = do let n = length p
                     c1 <- sampleTo (n-1)
                     c2 <- sampleTo (n-1)
                     return $ min (p !! c1) (p !! c2)

-- | Perform one iteration of ITEA
step :: (NFData a, NFData b) => Mutation a -> Fitness a b -> Int -> Population a b -> Rnd (Population a b)
step mutFun fitFun nPop pop = do
  let n = nPop `div` (2*numCapabilities)
  children  <- parRandom n (mutFun . _expr) fitFun pop
  --children  <- parMaybeMap n fitFun <$> traverse (mutFun . _expr) pop
  if null children
  then tournament pop nPop
  else tournament (pop ++ children) nPop
