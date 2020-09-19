{-|
Module      : IT.Mutation
Description : Mutation operators for ITEA
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

-}
module IT.Mutation where

import Data.Map.Strict as M

import IT -- (itea, addTerm, dropTerm)
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
addTerm :: Rnd (Term a) -> Mutation a
addTerm rndTerm e = do t <- rndTerm
                       if e `hasTerm` t
                       then return e
                       else return (t `consTerm` e)

-- | Create a Drop term mutation.
--
--      Drops a random term of the expression.
--
--      You need to provide a Fitness function.
--
dropTerm :: Mutation a
dropTerm e = do let n = numberOfTerms e
                i <- sampleTo (n-1)
                return (removeIthTerm i e)

-- | Create a Random Replace Term mutation
--
--         Replace one random strength of
--         a random term of the expression.
--         You need to provide the minimum 
--         and maximum allowed exponent
replaceTerm :: Int -> Int -> Int -> Mutation a
replaceTerm dim minExp maxExp e = do let n = numberOfTerms e
                                     i <- sampleTo (n-1)
                                     let t  = fromJust $ getIthTerm i e
                                         e' = removeIthTerm i e
                                     t' <- rndReplaceStrength dim t minExp maxExp
                                     return (t' `consTerm` e')
  where fromJust (Just x) = x
  
-- | replaces a strength at random
rndReplaceStrength :: Int -> Term a -> Int -> Int -> Rnd (Term a)
rndReplaceStrength dim (Term tf ps) minExp maxExp = 
  do p <- sampleRng minExp maxExp
     i <- sampleTo (dim-1)
     let ps' = M.filter (/=0) $ M.insert i p ps
     return (Term tf ps')
                                    
-- | replaces a random transformation function
replaceTrans :: Rnd (Transformation a) -> Mutation a
replaceTrans rndTrans e = do let n = numberOfTerms e
                             i  <- sampleTo (n-1)
                             tr <- rndTrans
                             return (replace i tr e)
  where
    change tr' (Term tr i)     = (Term tr' i)
    replace 0  tr (Expr [])    = Expr []
    replace 0  tr (Expr (t:e)) = Expr (change tr t : e)
    replace i  tr (Expr (t:e)) = t `consTerm` replace (i-1) tr (Expr e)

-- | Combine two interactions with `op` operation (use (+) or (-)
-- for positive and negative interaction)
combineInter :: (Int -> Int -> Int) -> Int -> Int -> Mutation a
combineInter op minExp maxExp e = do let n = numberOfTerms e
                                     i <- sampleTo (n-1)
                                     j <- sampleTo (n-1)
                                     let ti = fromJust $ getIthTerm i e
                                         tj = fromJust $ getIthTerm j e
                                         e' = removeIthTerm i e
                                         ti'= combineBoth ti tj
                                     if  allZeros ti'
                                     then return e'
                                     else return . uniqueTerms $ ti' `consTerm` e'
  where
    allZeros (Term _  is) = M.size is == 0
    fromJust (Just x)     = x
    
    combineBoth (Term tr1 int1) (Term _ int2) = Term tr1 (M.filter (/=0) $ M.unionWith (\i1 i2 -> minmax (i1 `op` i2)) int1 int2)
    minmax x = min maxExp $ max minExp $ x

-- | Positive and Negative interaction mutations
positiveInter = combineInter (+)
negativeInter = combineInter (-)

-- | Apply one of the mutation functions at random
mutFun :: Int                    -- ^ Dim
       -> (Int, Int)             -- ^ minExp, maxExp
       -> (Int, Int)             -- ^ minTerms, maxTerms
       -> Rnd (Term a)           -- ^ random term generator
       -> Rnd (Transformation a) -- ^ random term generator
       -> Expr a                 -- ^ Expression to be mutated
       -> Rnd (Expr a)           -- ^ Random Expression generator
mutFun dim (minExp, maxExp) (minTerms, maxTerms) rndTerm rndTrans e = sampleFromList muts >>= id
  where
    muts   = [replaceTerm dim minExp maxExp e
             ,replaceTrans rndTrans         e
             ,positiveInter minExp maxExp   e
             ,negativeInter minExp maxExp   e] ++ addMut ++ dropMut
             
    addMut  = if len <= maxTerms then [addTerm rndTerm e] else []
    dropMut = if len >= minTerms then [dropTerm e]        else []
    len     = numberOfTerms e
