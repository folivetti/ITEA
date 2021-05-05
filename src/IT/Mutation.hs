{-|
Module      : IT.Mutation
Description : Mutation operators for ITEA
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Mutation operators.
-}
module IT.Mutation where

import Data.List (nub)
import Data.IntMap.Strict as M
import Control.Monad

import IT
import IT.Algorithms
import IT.Random

-- ---------------------------------------------------------------------------


-- * Mutation builder functions


-- | Create an Add new term mutation.
--
--      Adds a random term into the expression.
--      If this term already exists in the expression,
--      it returns the original expression without modification.
--
--      You need to provide a Fitness function and a function that
--      samples a random term.
--
addTerm :: Rnd Term -> Mutation
addTerm rndTerm e = do t <- rndTerm
                       if t `elem` e
                       then return e
                       else return (t : e)

-- | Create a Drop term mutation.
--
--      Drops a random term of the expression.
--
--      You need to provide a Fitness function.
--
dropTerm :: Mutation
dropTerm e = do let n = length e
                i <- sampleTo (n-1)
                return (removeIthTerm i e)

-- | Create a Random Replace Term mutation
--
--         Replace one random strength of
--         a random term of the expression.
--         You need to provide the minimum 
--         and maximum allowed exponent
replaceTerm :: Int -> Int -> Int -> Mutation
replaceTerm dim minExp maxExp e = do let n = length e
                                     i <- sampleTo (n-1)
                                     let t  = fromJust $ getIthTerm i e
                                         e' = removeIthTerm i e
                                     t' <- rndReplaceStrength dim t minExp maxExp
                                     return (t' : e')
  where 
    fromJust (Just x) = x
    fromJust Nothing  = error "Couldn't get i-th term in replaceTerm"  -- this should never happen

-- | replaces a strength at random
rndReplaceStrength :: Int -> Term -> Int -> Int -> Rnd Term
rndReplaceStrength dim (Term tf ps) minExp maxExp =
  do p <- sampleRng minExp maxExp
     i <- sampleTo (dim-1)
     let ps' = M.filter (/=0) $ M.insert i p ps
     if M.size ps' == 0
       then return (Term tf ps)
       else return (Term tf ps')

-- | replaces a random transformation function
replaceTrans :: Rnd Transformation -> Mutation
replaceTrans rndTrans e = do let n = length e
                             i  <- sampleTo (n-1)
                             tr <- rndTrans
                             return (replace i tr e)
  where
    change tr' (Term _ i)       = Term tr' i
    replace 0  _  ([])     = []
    replace _  _  ([])     = error "Empty expression in replaceTrans"
    replace 0  tr ((t:es)) = (change tr t : es)
    replace i  tr ((t:es)) = t : replace (i-1) tr (es)

-- | Combine two interactions with `op` operation (use (+) or (-)
-- for positive and negative interaction)
combineInter :: (Int -> Int -> Int) -> Int -> Int -> Mutation
combineInter op minExp maxExp e = do let n = length e
                                     i <- sampleTo (n-1)
                                     j <- sampleTo (n-1)
                                     let ti = fromJust $ getIthTerm i e
                                         tj = fromJust $ getIthTerm j e
                                         e' = removeIthTerm i e
                                         ti'= combineBoth ti tj
                                     if  allZeros ti'
                                     then return e'
                                     else return . nub $ ti' : e'
  where
    allZeros (Term _  is) = M.size is == 0
    fromJust (Just x)     = x
    fromJust Nothing      = error "Couldn't get a term in combineInter"

    combineBoth (Term tr1 int1) (Term _ int2) = Term tr1 (M.filter (/=0) $ M.unionWith (\i1 i2 -> minmax (i1 `op` i2)) int1 int2)
    minmax x = min maxExp $ max minExp x

-- | Positive and Negative interaction mutations
positiveInter, negativeInter :: Int -> Int -> Mutation
positiveInter = combineInter (+)
negativeInter = combineInter (-)

-- | Apply one of the mutation functions at random
mutFun :: Int                    -- ^ Dim
       -> (Int, Int)             -- ^ minExp, maxExp
       -> (Int, Int)             -- ^ minTerms, maxTerms
       -> Rnd Term           -- ^ random term generator
       -> Rnd Transformation -- ^ random term generator
       -> Expr                 -- ^ Expression to be mutated
       -> Rnd Expr           -- ^ Random Expression generator
mutFun dim (minExp, maxExp) (minTerms, maxTerms) rndTerm rndTrans e = join (sampleFromList muts)
  where
    muts   = [replaceTerm dim minExp maxExp e
             ,replaceTrans rndTrans         e
             ,positiveInter minExp maxExp   e
             ,negativeInter minExp maxExp   e] ++ addMut ++ dropMut

    addMut  = [addTerm rndTerm e | len <  maxTerms]
    dropMut = [dropTerm e        | len >  minTerms]
    len     = length e
