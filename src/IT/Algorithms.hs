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

import Control.DeepSeq
import qualified Numeric.LinearAlgebra as LA

-- | data type containing a solution, its fitness and weight vector 
--  'a' refers to the type of 'Expr', 'b' refers to a container of statistics.
data Solution = Sol { _expr    :: Expr     -- ^ The IT expression of type a
                    , _fit     :: [Double] -- ^ Fitness and other measures for evaluating the expression
                    , _weights :: [Vector]
                    }

instance Show Solution where
  show (Sol e f w) = "Expression: " ++ toExprStr e (LA.toList $ head w) ++ "\nFitness: " ++ (show . head) f ++ "\nWeights: " ++  show w
  
-- | These instances are only to find the best and worst individuals
-- of a population.
instance Eq Solution where
  -- | 'Eq' instance to sort a sequence
  -- of solutions by fitness
  s1 == s2 = (head._fit) s1 == (head._fit) s2

instance Ord Solution where
  -- | 'Ord' instance to sort a sequence
  -- of solutions by fitness
  s1 <= s2 = (head._fit) s1 <= (head._fit) s2

-- | A population of 'Solution a b'
type Population = [Solution]

instance NFData Solution where
  rnf _ = ()

-- | 'Fitness' function that takes a list of expressions and 
-- returns an evaluated population. 
-- This function is a good candidate for parallelization.
--type Fitness    a b = [Expr a] -> Population a b -- (Expr a, Double, b)
type Fitness = Expr -> Maybe Solution

-- | 'Mutation' function with signature 'Solution a b -> Rnd (Solution a b)'
type Mutation = Expr -> Rnd Expr
