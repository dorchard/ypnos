{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ypnos.CUDA.Expr.Fun where

import Ypnos.Core.Grid

import Data.Maybe
import Data.Generics
import Language.Haskell.TH 
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Ypnos.Expr.Expr
import Data.Ix
import Data.Array.IArray

import Debug.Trace

--The Parser Bit--

fvar = varE . mkName
fcon = conE . mkName

fun :: QuasiQuoter
fun = QuasiQuoter { quoteExp = quoteExprExp,
                   quotePat = quoteExprPat,
                   quoteType = undefined,
                   quoteDec = undefined
                 }

quoteExprExp :: String -> ExpQ
quoteExprExp input = do loc <- location
                        let pos = (loc_filename loc,
                               fst (loc_start loc),
                               snd (loc_start loc))
                        expr <- (parseExpr gridFun) pos input
                        dataToExpQ (const Nothing `extQ` interpret) expr


quoteExprPat :: String -> PatQ
quoteExprPat input = do loc <- location
                        let pos = (loc_filename loc,
                               fst (loc_start loc),
                               snd (loc_start loc))
                        expr <- (parseExpr gridFun) pos input
                        dataToPatQ (const Nothing) expr

interpret :: GridFun -> Maybe (Q Exp)
interpret (GridFun pattern body) =
   case parseExp body of
     Left x -> error x
     Right bodyExpr -> Just gridFun
         where
           gridFun = lamE [gpat] (return bodyExpr)
           gpat = mkPattern pattern 

-- We make a pattern in 3 steps:
-- Ypnos Grid pattern -> Convert to intermediate representation ->
-- Centre the grid in this representation -> Convert to pattern
mkPattern' :: GridIx i => GridPattern' i -> PatQ
mkPattern' = intToPatt . centreCursor . ypToInt

-- We must convert to to GridPattern' where the dimensionality is in the type
-- (above).
mkPattern :: GridPattern -> PatQ
mkPattern (GridPattern1D _ vs) = mkPattern' $ GridPattern1D' vs
mkPattern (GridPattern2D _ _ vs) = mkPattern' $ GridPattern2D' vs


-- Intermediate representation
data Intermediate i where 
    Inter :: Ix i => i -> Array i VarP -> Intermediate i

-- Grid pattern with dimensionality in type.
data family GridPattern' i
data instance GridPattern' (Int) = 
    GridPattern1D' [VarP]
data instance GridPattern' (Int, Int) = 
    GridPattern2D' [[VarP]]

-- Various ad-hoc polymorphic helper functions. These help us deal with both 1D
-- and 2D pattern simultaneously.
class Ix i => GridIx i where
    getCurIx :: GridPattern' i -> i
    getBounds :: GridPattern' i -> (i,i)
    safeConcat :: GridPattern' i -> [VarP]
    seperateBounds :: Array i e -> ((Int, Int),(Int,Int))
    ix :: (Int, Int) -> i 

instance GridIx Int where
    getBounds (GridPattern1D' ls) = 
        (0::Int, length ls - 1)
    getCurIx (GridPattern1D' _) = 1 --TODO: Implement
    safeConcat (GridPattern1D' ls) = ls
    seperateBounds arr = ((fa, la),(fb,lb))
        where (fa, la) = bounds arr
              (fb, lb) = (0, 0)
    ix (i, j) = i

instance GridIx (Int, Int) where
    getBounds (GridPattern2D' ls) = 
        ((0::Int,0::Int), (length (head ls) -1, length ls -1))
    getCurIx (GridPattern2D' _) = (1,1) --TODO: Implement for real
    safeConcat (GridPattern2D' ls) = concat ls
    seperateBounds arr = ((fa, la),(fb,lb))
        where ((fa,fb),(la,lb)) = bounds arr
    ix = id
    
-- Stage 1: converting to intermediate
ypToInt :: GridIx i => GridPattern' i -> Intermediate i
ypToInt pat = 
        let range = getBounds pat 
            ls = safeConcat pat 
        in
        Inter (getCurIx pat) 
              (listArray range ls)

-- Stage 2: centering the cursor
centreCursor :: Intermediate i -> Intermediate i
centreCursor = id

-- Stage 3: converting to pattern
intToPatt :: (GridIx i) => Intermediate i -> PatQ
intToPatt (Inter _ arr) =
    tupP [
        tupP [
            getName (arr ! (ix (i, j)))
                | i <- range a]
                    | j <- range b]
    where (a, b) = seperateBounds arr

getName :: VarP -> PatQ
getName v = 
    case uncurse v of
      PatternVar x -> varP $ mkName x
      PatternBlank -> wildP
    
uncurse :: VarP -> VarP'
uncurse (Cursor v) = v
uncurse (NonCursor v) = v
