{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : IT.Random
Description : random generation of IT components
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

-}
module IT.Random where

import IT

import System.Random
import System.Random.SplitMix
import Control.Monad.State
import qualified Data.Map.Strict as M

-- * Random expressions generation
 
-- | The type used for generating random values of 'a'
type Rnd a = State SMGen a

-- | Creates a random interaction with up to n variables. 
--   Guaranteed to choose at least one variable.
sampleInterMax :: Int               -- ^ problem dimension
               -> Int               -- ^ maximum number of variables in this interaction
               -> Int               -- ^ minimum exponent            
               -> Int               -- ^ maximum exponent
               -> Rnd Interaction   -- ^ Random interaction generator
sampleInterMax dim budget minExp maxExp = do es <- sampleInterMax' dim budget minExp maxExp
                                             return $ M.fromList es
                                          
sampleInterMax' 0   _      _      _      = return []
sampleInterMax' dim 0      minExp maxExp = return []
sampleInterMax' 1   budget minExp maxExp = do e <- sampleNZRng minExp maxExp
                                              return $ [(0,e)]
sampleInterMax' dim budget minExp maxExp = do b <- toss
                                              if b
                                              then do e  <- sampleNZRng minExp maxExp
                                                      es <- sampleInterMax' (dim-1) (budget-1) minExp maxExp
                                                      return $ ((dim-1,e):es)
                                              else do es <- sampleInterMax' (dim-1) budget minExp maxExp
                                                      return $ es

sampleInter :: Int               -- ^ problem dimension
            -> Int               -- ^ minimum exponent            
            -> Int               -- ^ maximum exponent
            -> Rnd Interaction   -- ^ Random interaction generator
sampleInter dim minExp maxExp = do es <- sampleInter' dim minExp maxExp
                                   return $ M.fromList es
                                
sampleInter' 0   _      _      = return []
sampleInter' dim minExp maxExp = do e  <- sampleRng minExp maxExp
                                    es <- sampleInter' (dim-1) minExp maxExp
                                    if e==0
                                    then return es
                                    else return ((dim-1,e):es)
                                          
                                          
-- | Samples a random transformation function from a provided list of functions
sampleTrans :: [Transformation a]      -- ^ choices of transformation functions
            -> Rnd (Transformation a)  -- ^ Random generator
sampleTrans ts = sampleFromList ts

-- | Samples a random term using a random transformation and a random interaction generators.
sampleTerm :: Rnd (Transformation a)  -- ^ random transformation function
           -> Rnd Interaction         -- ^ random interaction function
           -> Rnd (Term a)            -- ^ Random generator
sampleTerm rndTrans rndInter = do t <- rndTrans
                                  i <- rndInter
                                  return $ Term t i

-- | Create a random expression with exactly n terms
sampleExpr :: Rnd (Term a)         -- ^ random term function
           -> Int                  -- ^ number of terms
           -> Rnd (Expr a)         -- ^ Random generator
sampleExpr _ 0            = return (Expr [])
sampleExpr rndTerm nTerms = do t <- rndTerm 
                               e <- sampleExpr rndTerm (nTerms-1)
                               return $ t `consTerm` e

-- | Create a random population of n expressions with varying number of terms
samplePop :: Int                   -- population size
          -> Int                   -- max number of terms
          -> (Int -> Rnd (Expr a)) -- random expression generator
          -> Rnd [Expr a]
samplePop nPop maxNTerms rndExpr = do n  <- sampleRng 1 maxNTerms
                                      e  <- rndExpr n
                                      es <- samplePop (nPop-1) maxNTerms rndExpr
                                      return $ e : es

-- * Utility random functions

-- | sample from [0,n]
sampleTo :: Int -> Rnd Int
sampleTo n | n < 0     = error "Invalid number"
           | otherwise = sampleRng 0 n

-- | Sample from a range of integers
sampleRng :: Int -> Int -> Rnd Int
sampleRng x y = addBias  <$>  (state $ bitmaskWithRejection64 range)
  where addBias z = fromIntegral z + x
        range     = fromIntegral (y - x + 1)
-- randomR (x,y)

-- | Sample from a range of integers excluding 0
sampleNZRng :: Int -> Int -> Rnd Int
sampleNZRng x y = do z <- sampleRng x y
                     if z == 0 
                     then sampleNZRng x y
                     else return z

-- | Sample a random element from a list
sampleFromList :: [a] -> Rnd a
sampleFromList xs = do let n = length xs
                       i <- sampleTo (n-1)
                       return (xs !! i)

-- | Toss a coin to your Witcher
toss :: Rnd Bool
toss = state random
