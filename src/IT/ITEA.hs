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
where 'mutFun' is a mutation function of the type 'Mutation',
a 'fitness' function of type 'Fitness',
and 'pop0' is the initial 'Population' of solutions.
This function will result in an infinite list of populations, with
the /i/-th element being the population of the /i/-th generation.

This library also provides some generic mutation function builders.
-}
module IT.ITEA where

import IT
import IT.Algorithms
import IT.Random

import Control.Monad.Extra (iterateM)
import GHC.Conc (numCapabilities)

import Control.Monad.State
import Control.Parallel.Strategies
import Data.Maybe
import System.Random
import Data.List (nub)
import qualified Data.Sequence as Seq

-- * ITEA

-- | Creates a stream of generations the /i/-th 
-- element corresponds to the population of the /i/-th generation.
itea :: Mutation -> Fitness -> Population -> Rnd [Population]
itea f g p0 = let n = length p0
                      in  iterateM (step f g n) p0

-- | Generate an Initial Population at Random
initialPop :: Int                -- ^ maxTerms
           -> Int                -- ^ nPop
           -> Rnd Term       -- ^ random term generator
           -> Fitness          -- ^ fitness function
           -> Rnd Population
initialPop maxTerms nPop rndTerm fit = 
  do pop <- traverse rndIndividual $ replicate nPop ()
     parRndMap nPop (return . _expr) fit pop
  where
    rndExpr = sampleExpr rndTerm
    createSol e = Sol e [] 0.0 0 0.0 []

    -- return a random list of random expressions
    rndIndividual () = do n <- sampleRng 1 maxTerms
                          createSol . nub <$> rndExpr n


-- | Tournament Selection
--
-- given the concatenation of the previous population
-- and the mutated children, it will return a sampled
-- selection of these combined population with
-- the same size as the original population.
--
tournamentSeq :: Population -> Int -> Rnd Population
tournamentSeq [] _ = return []
tournamentSeq p n = do let p'   = Seq.fromList p
                           npop = Seq.length p'
                           chooseOne ix1 ix2 = min (p' `Seq.index` ix1) (p' `Seq.index` ix2)
                       ixs1 <- replicateM n (sampleTo (npop-1))
                       ixs2 <- replicateM n (sampleTo (npop-1))
                       return $ zipWith chooseOne ixs1 ixs2

-- | For small population, do not convert to Finger Tree to avoid overhead
tournament :: Population -> Int -> Rnd Population
tournament [] _ = return []
tournament p n = do let npop = length p
                        chooseOne ix1 ix2 = min (p !! ix1) (p !! ix2)
                    ixs1 <- replicateM n (sampleTo (npop-1))
                    ixs2 <- replicateM n (sampleTo (npop-1))
                    return $ zipWith chooseOne ixs1 ixs2

-- | Perform one iteration of ITEA
step :: Mutation -> Fitness -> Int -> Population -> Rnd Population
step mutFun fitFun nPop pop = do
  let tourn = if nPop >= 1000 then tournamentSeq else tournament
      mutf s = mutFun (_expr s)
  children <- mapMaybe fitFun <$> traverse mutf pop
  if null children
     then tourn pop nPop
     else tourn (pop <> children) nPop

-- | EXPERIMENTAL: step function with parallel evaluation 
stepPar :: Mutation -> Fitness -> Int -> Population -> Rnd Population
stepPar mutFun fitFun nPop pop = do
  let tourn  = if nPop >= 1000 then tournamentSeq else tournament
      mutf s = mutFun (_expr s)
  children  <- parRndMap nPop mutf fitFun pop
  if null children
   then tourn pop nPop
   else tourn (pop <> children) nPop

-- * Parallel random functions

-- | Runs in parallel the composition of a function that generates random effects with
-- a function that maybe returns a result.
--parRndMap :: NFData c => Int -> (a -> Rnd b) -> (b -> Maybe c) -> [a] -> Rnd [c]
--parRndMap :: Int -> Mutation -> (Solution -> Maybe Expr) -> [Solution] -> Rnd [Solution]
parRndMap :: Int -> (Solution -> Rnd Expr) -> (Expr -> Maybe Solution) -> [Solution] -> Rnd [Solution]
parRndMap nPop rndf randFun pop = state stFun
  where
    stFun seed = let seeds         = genNseeds (nPop+1) seed
                     rndpop        = zip seeds pop
                     compFun (s,p) = randFun $ evalState (rndf p) s
                     nSplits       = numberOfSplits nPop
                     pop'          = parMaybeMap nSplits compFun rndpop
                 in  (pop', last seeds)

-- | Calculates the number of splits as twice the number of cores
numberOfSplits :: Int -> Int
numberOfSplits n = n `div` 2*numCapabilities

-- | Generates n random seeds.
genNseeds :: Int -> StdGen -> [StdGen]
genNseeds n = take n . genseeds

-- | Generates an infinite list of random seeds.
genseeds :: StdGen -> [StdGen]
genseeds s = let (s1, s2) = split s
             in  s1 : genseeds s2

-- | Runs a computation that may returns a result in parallel.
--parMaybeMap :: NFData b => Int -> (a -> Maybe b) -> [a] -> [b]
parMaybeMap :: Int -> ((StdGen, Solution) -> Maybe Solution) -> [(StdGen, Solution)] -> Population
parMaybeMap n f pop = catMaybes parmap
  where
    parmap = map f pop `using` parListChunk n rpar

