{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : IT
Description : IT expression data structures
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

An IT expression  represents a function of the form:

\[
f(x) = \sum_{i}{w_i \cdot t_i(\prod_{j}{x_j^{k_{ij}})}}
\]

with \(t_i\) being a transformation function.

Any given expression can be represented by a list of terms, with each term
being composed of a transformatioon function and an interaction.
The transformation function is represented by a `String` of its name and
a function of the type `a -> a`.
The interaction is represented as a `Map Int Int` where the key is the 
predictor index and the value is the strength of the predictor in this
term. The strengths with a value of zero are omitted.
-}
module IT where

import Foreign.Storable

import Data.List (intercalate, nub)

import qualified Data.Map.Strict as M
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V

-- | The 'Interaction' type is a map where
-- a key, value pair (i,p) indicates that the i-th
-- variable should be raised to the power of p.
type    Interaction      = M.Map Int Int

-- | The 'Transformation' type contains the name of the function
-- and the transformation function.
data    Transformation a = Transformation String (a -> a)

-- | A 'Term' is the product type of a 'Transformation' and an 'Interaction'.
data    Term a           = Term (Transformation a) Interaction

-- | An 'Expr' is just a list of 'Term's.
newtype Expr a           = Expr [Term a]

-- | A 'Column' of a data set is stored as a LA.Vector to avoid conversions
-- during the fitting of the coefficients.
type Column  a = LA.Vector a

-- | The 'Dataset' is a 'Vector' of 'Column's for efficiency.
-- TODO: use Accelerate for large data sets.
type Dataset a = V.Vector (Column a)

-- * Class 'IT' of IT-expressions

-- | The 'IT' class defines how we should
-- evaluate an expression given the type 'a'.
--
class IT a where
  -- | 'itTimes' evaluates the interaction of an expression using the values in the 'Dataset', it returns a single 'Column' representing the new predictor.
  itTimes  :: Dataset a -> Interaction -> Column a
  -- | 'itAdd' sums a list of terms.
  itAdd    :: [Column a] -> Column a
  -- | 'itWeight' applies the coefficients to a 'Column'.
  itWeight :: Double -> Column a -> Column a

-- | to evaluate a term we apply the transformation function
-- to the result of 'itTimes'.
evalTerm :: (Storable a, LA.Container LA.Vector a, IT a) => Term a -> Dataset a -> Column a
evalTerm (Term (Transformation _ f) is) ds =  LA.cmap f (itTimes ds is)

-- | evaluates the expression into a list of terms
-- in case the evaluated values are needed
evalExprToList :: (Storable a, LA.Container LA.Vector a, IT a) => Expr a -> Dataset a -> [Column a]
evalExprToList (Expr es) xs = map (\t -> evalTerm t xs) es

-- | evaluates an expression by evaluating the terms into a list
-- applying the weight and summing the results.
evalExpr :: (Storable a, LA.Container LA.Vector a, IT a) => Expr a -> Dataset a -> [Double] -> Column a
evalExpr es xs ws = itAdd $ zipWith itWeight ws (evalExprToList es xs)
  
-- 'Show' and 'Eq' instances
-- obs.: maybe this makes more sense specifically for each instance of IT
instance (Show a) => Show (Expr a) where
  show (Expr es) = intercalate " + " $ map show es

instance Show a => Show (Term a) where
   show (Term tr i)   = show tr ++ "(" ++ showInter i ++ ")" 
     where
       -- String representation of an interaction term.
       -- TODO: replace variable names with provided labels
       showInter :: M.Map Int Int -> String
       showInter es = (intercalate "*" . filter (/="") . map show') 
                    $ M.toList es
         where 
           show' (n, 0) = ""
           show' (n, 1) = 'x' : show n
           show' (n, e) = ('x' : show n) ++ "^(" ++  show e ++ ")"

instance Show a => Show (Transformation a) where
  show (Transformation s _) = s

          
-- | Two terms are equal if their interactions are equal
-- this instance is used for the mutation operation to avoid adding 
-- two interactions with the same value on the same expression. 
instance Eq (Term a) where
  (Term tr1 i1) == (Term tr2 i2) = (i1 == i2)

instance Eq (Transformation a) where
  (Transformation s1 _) == (Transformation s2 _) = s1==s2
  
-- * Utility functions

-- | Remove duplicated terms.
uniqueTerms :: Expr a -> Expr a
uniqueTerms (Expr ts) = Expr (nub ts)

-- | Check whether and expression has a given term.
hasTerm :: Expr a -> Term a -> Bool
hasTerm (Expr ts) t = t `elem` ts


-- * Internal functions

-- | remove the i-th term of an expression.
removeIthTerm :: Int -> Expr a -> Expr a
removeIthTerm i (Expr e) = Expr (take i e ++ drop (i+1) e)

-- | returns the i-th term of an expression.
getIthTerm :: Int -> Expr a -> Maybe (Term a)
getIthTerm ix (Expr e) = if   ix >= length e
                         then Nothing
                         else Just (e !! ix)

-- | inserts a new term into the expression.
consTerm :: Term a -> Expr a -> Expr a
consTerm t (Expr e) = Expr (t:e)

-- | get the list of terms
getListOfTerms :: Expr a -> [Term a]
getListOfTerms (Expr ts) = ts

-- | returns the number of terms in an expression.
numberOfTerms :: Expr a -> Int
numberOfTerms (Expr ts) = length ts

-- | return the interactions of a term
getInteractions :: Term a -> Interaction
getInteractions (Term _ is) = is
