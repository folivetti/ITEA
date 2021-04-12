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

import Data.List (intercalate)

import qualified Data.IntMap.Strict as M
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V

-- | The 'Interaction' type is a map where
-- a key, value pair (i,p) indicates that the i-th
-- variable should be raised to the power of p.
type Interaction = M.IntMap Int

-- | The 'Transformation' type contains the name of the function
-- and the transformation function.
data Transformation = Id | Sin | Cos | Tan | Tanh | Sqrt | SqrtAbs | Log | Exp | Log1p
                        deriving (Show, Read, Eq, Ord)

-- | A 'Term' is the product type of a 'Transformation' and an 'Interaction'.
data Term = Term Transformation Interaction
              deriving (Show, Read)

-- | An 'Expr' is just a list of 'Term's.
type Expr = [Term]

-- | A 'Column' of a data set is stored as a LA.Vector to avoid conversions
-- during the fitting of the coefficients.
type Column a = LA.Vector a

-- | The 'Dataset' is a 'Vector' of 'Column's for efficiency.
-- TODO: use Accelerate for large data sets.
type Dataset a = V.Vector (Column a)

-- 'Show' and 'Eq' instances
-- obs.: maybe this makes more sense specifically for each instance of IT


prettyPrint :: ((Int, Int) -> String) -> (Transformation -> String) -> Expr -> [Double] -> String 
prettyPrint _ _ [] _ = ""
prettyPrint _ _ _ [] = error "ERROR: prettyPrint on non fitted expression."
prettyPrint k2str t2str terms (b:ws) = show b ++ " + " ++ expr
  where
    expr = intercalate " + " (zipWith weight2str ws (map terms2str terms))

    interaction2str       = intercalate "*" . filter (/="") . map k2str . M.toList
    terms2str (Term t ks) = t2str t ++ "(" ++ interaction2str ks ++ ")"
    weight2str w t        = show w ++ "*" ++ t

toExprStr :: Expr -> [Double] -> String
toExprStr = prettyPrint k2str show
  where 
    k2str (_, 0) = ""
    k2str (n, 1) = 'x' : show n
    k2str (n, k) = ('x' : show n) ++ "^(" ++ show k ++ ")"

toPython :: Expr -> [Double] -> String
toPython = prettyPrint k2str numpy
  where
    k2str (_, 0) = ""
    k2str (n, 1) = "x[:," ++ show n ++ "]"
    k2str (n, k) = "x[:," ++ show n ++ "]" ++ "**(" ++ show k ++ ")"

    numpy Id      = ""
    numpy Sin     = "np.sin"
    numpy Cos     = "np.cos"
    numpy Tan     = "np.tan"
    numpy Tanh    = "np.tanh"
    numpy Sqrt    = "np.sqrt"
    numpy SqrtAbs = "sqrtAbs"
    numpy Exp     = "np.exp"
    numpy Log     = "np.log"
    numpy Log1p   = "np.log1p"

-- | Two terms are equal if their interactions are equal
-- this instance is used for the mutation operation to avoid adding 
-- two interactions with the same value on the same expression. 
instance Eq Term where
  (Term _ i1) == (Term _ i2) = i1 == i2

-- * Internal functions

-- | remove the i-th term of an expression.
removeIthTerm :: Int -> Expr -> Expr 
removeIthTerm i terms = take i terms ++ drop (i+1) terms

-- | returns the i-th term of an expression.
getIthTerm :: Int -> Expr -> Maybe Term
getIthTerm ix terms = if ix >= length terms 
                       then Nothing
                       else Just (terms !! ix)

-- | return the interactions of a term
getInteractions :: Term -> Interaction
getInteractions (Term _ ks) = ks

-- | returns the length of an expression as in https://github.com/EpistasisLab/regression-benchmark/blob/dev/CONTRIBUTING.md
exprLength :: Expr -> [Double] -> Int
exprLength [] _ = 0
exprLength _ [] = error "ERROR: length of unfitted expression."
exprLength terms (b:ws) = biasTerm + addSymbs + weightSymbs + totTerms
  where
    nonNullTerms = filter ((/=0).fst) $ zip ws terms
    ws'          = map fst nonNullTerms
    terms'       = map snd nonNullTerms
    biasTerm     = if b==0 then 0 else 2
    weightSymbs  = 2 * length (filter (/=1) ws')
    addSymbs     = length terms' - 1
    totTerms     = sum (map termLength terms)
    
termLength :: Term -> Int
termLength (Term Id ks) = interactionLength ks
termLength (Term _  ks) = 1 + interactionLength ks

interactionLength :: Interaction -> Int
interactionLength ks = mulSymbs + termSymbs + powSymbs
  where
    elems     = filter (/=0) $ M.elems ks
    mulSymbs  = length elems - 1
    termSymbs = length elems
    powSymbs  = 2 * length (filter (/=1) elems)
