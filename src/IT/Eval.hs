{-|
Module      : IT.Regression
Description : Specific functions for Symbolic Regression
Copyright   : (c) Fabricio Olivetti de Franca, 2020
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

Definitions of IT data structure and support functions.
-}

module IT.Eval where

import Data.Semigroup
import qualified Data.List.NonEmpty as NE

import qualified Data.Vector as VV
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Map.Strict as M

import IT
import IT.Algorithms

-- | IT Instance for regression evals an expression to
-- sum(w_i * term_i) + w0
-- with
--      term_i = f_i(p_i(xs))
--  and p_i(xs) = prod(x^eij)
instance IT Double where
  --itTimes :: Dataset Double -> Interaction -> Column Double
  itTimes vs m
    | VV.length vs == 0 = LA.fromList []
    | otherwise         = M.foldrWithKey f vzero m
    where
      vzero    = n LA.|> repeat 1
      f ix p v = v * LA.cmap (^^p) (vs VV.! ix)
      n        = LA.size (vs VV.! 0)

  --itAdd :: [Column Double] -> Column Double
  itAdd = getSum . foldMap Sum

  --itWeight :: Double -> Column Double -> Column Double
  itWeight w = LA.cmap (w*)

-- | Transformation Functions
regSin, regCos, regTan, regTanh, regSqrt, regAbsSqrt, regLog, regExp :: Transformation Double
regSin     = Transformation "sin" sin
regCos     = Transformation "cos" cos
regTan     = Transformation "tan" tan
regTanh    = Transformation "tanh" tanh

regSqrt    = Transformation "sqrt" sqrt
regAbsSqrt = Transformation "sqrt.abs" (sqrt.abs)
regLog     = Transformation "log" log
regExp     = Transformation "exp" exp

regTrig      = [regSin, regCos, regTanh] -- regTan
regNonLinear = [regExp, regLog, regAbsSqrt] -- regSqrt regExp
regLinear    = [Transformation "id" id]

regAll = regTrig ++ regNonLinear ++ regLinear

-- | A value is invalid if it's wether NaN or Infinite
isInvalid :: Double -> Bool
isInvalid x = isNaN x || isInfinite x

-- | a set of points is valid if none of its values are invalid and
-- the maximum abosolute value is below 1e150 (to avoid overflow)
isValid :: [Double] -> Bool
isValid xs = not (any isInvalid xs) && (maximum (map abs xs) < 1e150)

{- TO BE TESTED
 -- && var > 1e-4
  where
    mu  = sum(xs) / n
    var = (*(1/n)) . sum $ map (\x -> (x-mu)*(x-mu)) xs
    n   = fromIntegral (length xs)
-}

-- | evaluate an expression to a set of samples 
--
-- (1 LA.|||) adds a bias dimension
exprToMatrix :: Dataset Double -> Expr Double -> LA.Matrix Double
exprToMatrix rss (Expr e) = ((1 LA.|||) . LA.fromColumns) zss
  where
    zss = map (`evalTerm` rss) e

-- | Clean the expression by removing the invalid teerms
cleanExpr :: Dataset Double -> Expr Double -> Expr Double
cleanExpr rss (Expr e) = Expr (cleanExpr' e)
  where
    cleanExpr' [] = []
    cleanExpr' (t:e) = if ((/=[]) . LA.find isInvalid . evalTerm t) rss
                       then cleanExpr' e
                       else t : cleanExpr' e

-- | Checks if the fitness of a solution is not Inf nor NaN.
notInfNan :: Solution Double -> Bool
notInfNan s = not (isInfinite f || isNaN f)
  where f = NE.head $ _fit s

