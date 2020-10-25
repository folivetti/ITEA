{-|
Module      : IT.Algorithms
Description : Basic algorithms data structures
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Definitions  for a Solution, Population of solutions, 
fitness function and mutation function.
-}
module IT.Algorithms where

import IT
import IT.Random
import IT.Metrics

import Data.List.NonEmpty as NE
import Control.DeepSeq

-- | data type containing a solution, its fitness and weight vector 
--  'a' refers to the type of 'Expr', 'b' refers to a container of statistics.
data Solution a = Sol { _expr    :: Expr a     -- ^ The IT expression of type a
                      , _fit     :: NonEmpty Double   -- ^ Fitness and other measures for evaluating the expression
                      , _weights :: Vector
                      }

instance Show a => Show (Solution a) where
  show (Sol e f w) = "Expression: " ++ show e ++ "\nFitness: " ++ (show . NE.head) f ++ "\nWeights: " ++  show w
  
-- | These instances are only to find the best and worst individuals
-- of a population.
instance Eq (Solution a) where
  -- | 'Eq' instance to sort a sequence
  -- of solutions by fitness
  s1 == s2 = (NE.head._fit) s1 == (NE.head._fit) s2

instance Ord (Solution a) where
  -- | 'Ord' instance to sort a sequence
  -- of solutions by fitness
  s1 <= s2 = (NE.head._fit) s1 <= (NE.head._fit) s2

-- | A population of 'Solution a b'
type Population a = [Solution a]

instance NFData a => NFData (Solution a) where
  rnf _ = ()

-- | 'Fitness' function that takes a list of expressions and 
-- returns an evaluated population. 
-- This function is a good candidate for parallelization.
--type Fitness    a b = [Expr a] -> Population a b -- (Expr a, Double, b)
type Fitness    a = Expr a -> Maybe (Solution a)

-- | 'Mutation' function with signature 'Solution a b -> Rnd (Solution a b)'
type Mutation   a   = Expr a -> Rnd (Expr a)
