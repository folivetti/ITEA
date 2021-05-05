{-|
Module      : IT.FI2POP 
Description : Feasible-Infeasible Two-Population Interaction-Transformation Evolutionary Algorithm
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Generic implementation of Interaction-Transformation Evolutionary Algorithm
for any instance of IT expression with Feasible-Infeasible Two-Population for
constraint handling.

To run itea you just need to call 'fi2pop mutFun fitness (pop0-feasible, pop0-infeasible)', 
where 'mutFun' is a mutation function of the type 'Mutation',
'fitness' is a fitness function of type 'Fitness',
and 'pop0-x' is the initial 'Population' of solutions of feasible or infeasible .
This function will result in an infinite list of populations, with
the /i/-th element being the population of the /i/-th generation.

This library also provides some generic mutation function builders.
-}
module IT.FI2POP where

import IT.Algorithms
import IT.Random
import IT.ITEA

import Control.Monad.Extra (iterateM)

-- * FI2POP

-- | Creates a stream of generations the /i/-th 
-- element corresponds to the population of the /i/-th generation.
fi2pop :: Mutation -> Fitness -> (Population, Population) -> Rnd [(Population, Population)]
fi2pop f g (feas0, infeas0) = let n = length feas0 -- + length infeas0
                              in  iterateM (step2pop f g n) (feas0, infeas0)

-- | Splits the population as feasible and infeasible. 
splitPop :: Population -> (Population, Population)
splitPop pop = go pop [] []
  where
    go []     feas infeas = (feas, infeas)
    go (p:ps) feas infeas
      | _constr p == 0 = go ps (p:feas) infeas
      | otherwise      = go ps feas (p:infeas)
      
-- | Performs one iteration of FI2POP
step2pop :: Mutation -> Fitness -> Int -> (Population, Population) -> Rnd (Population, Population)
step2pop mutFun fitFun nPop (feas, infeas) = do
  let tourn = if nPop >= 1000 then tournamentSeq else tournament
  childrenF <- parRndMap nPop (mutFun . _expr) fitFun feas
  childrenI <- parRndMap nPop (mutFun . _expr) fitFun infeas
  let (feas', infeas') = splitPop (childrenF ++ childrenI)
  nextFeas <- tourn (feas ++ feas') nPop
  nextInfeas <- tourn (infeas ++ infeas') nPop  
  return (nextFeas, nextInfeas)

-- TODO: test it \/
{-
      nFeas            = max (length feas) (length feas')
      nInfeas          = max (length infeas) (length infeas')
      halfPop          = nPop `div` 2
  nextFeas <- if null feas'
                then tournament feas nFeas
                else tournament (feas ++ feas') (min nFeas halfPop)
  nextInfeas <- if null infeas'
                  then tournament infeas nInfeas
                  else tournament (infeas ++ infeas') (min nInfeas halfPop)
-}
