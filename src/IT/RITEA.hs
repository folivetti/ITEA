{-|
Module      : IT.RITEA
Description : Rational Interaction-Transformation Evolutionary Algorithm
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
module IT.RITEA where

import IT
import IT.Algorithms
import IT.Random
import IT.ITEA

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
ritea :: RatioMutation -> Fitness -> Population -> Rnd [Population]
ritea mut fit p0 = let n = length p0
                   in  iterateM (stepRITEA mut fit n) p0


-- | Perform one iteration of RITEA
stepRITEA :: RatioMutation -> Fitness -> Int -> Population -> Rnd Population
stepRITEA mutFun fitFun nPop pop = do
  let tourn  = if nPop >= 1000 then tournamentSeq else tournament
      mutf s = mutFun (_expr s) (_ratio s)
  children <- mapMaybe (uncurry fitFun) <$> traverse mutf pop
  if null children
     then tourn pop nPop
     else tourn (pop <> children) nPop

