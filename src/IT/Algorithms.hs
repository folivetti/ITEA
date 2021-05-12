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
                    , _constr  :: Double   -- ^ Amount of Shape Constraint violation associated with the expression, always positive
                    , _len     :: Int      -- ^ Expression size as per https://github.com/EpistasisLab/regression-benchmark/blob/dev/CONTRIBUTING.md
                    , _penalty :: Double   -- ^ penalty of fitness
                    , _weights :: [Vector] -- ^ Weights associated with the expression (they count towards the length)
                    }

instance Show Solution where
  show (Sol e f c l _ w) = concat ["Expression: "  , expr,    "\n"
                                , "Fitness: "    , fit,     "\n"
                                , "Weights: "    , weights, "\n"
                                , "Constraints: ", constr,  "\n"
                                , "Length: "     , len,     "\n"]
    where
      expr    = toExprStr e (LA.toList $ head w)
      fit     = (show . head) f
      weights = show w
      constr  = show c
      len     = show l
  
-- | These instances are only to find the best and worst individuals
-- of a population.
instance Eq Solution where
  -- | 'Eq' instance to sort a sequence
  -- of solutions by fitness
  s1 == s2 = (head._fit) s1 + _penalty s1 == (head._fit) s2 + _penalty s2

instance Ord Solution where
  -- | 'Ord' instance to sort a sequence
  -- of solutions by fitness
  s1 <= s2 = (head._fit) s1 + _penalty s1 <= (head._fit) s2 + _penalty s2

-- | A population of 'Solution a b'
type Population = [Solution]

instance NFData Solution where
  rnf _ = ()

-- | 'Fitness' function that takes a list of expressions and 
-- returns an evaluated population. 
-- This function is a good candidate for parallelization.
--type Fitness    a b = [Expr a] -> Population a b -- (Expr a, Double, b)
type Fitness = Expr -> Maybe Solution

-- | 'Constraint' is a function that receives an expression and its coefficients
-- and return the penalty associated with the constraint violation.
type Constraint = Expr -> [Double] -> Double

-- | 'Mutation' function with signature 'Solution a b -> Rnd (Solution a b)'
type Mutation = Expr -> Rnd Expr
