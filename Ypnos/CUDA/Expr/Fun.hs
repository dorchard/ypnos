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

import Data.List hiding (find)

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
interpret (GridFun pat body) =
   case parseExp body of
     Left x -> error x
     Right bodyExpr -> Just gridFun
         where
           gridFun = lamE [gpat] (return bodyExpr)
           gpat = pattern pat 

-- Before converting into the patq representation we must first centre the
-- pattern for accelerate.
pattern' :: GridIx i => GridPatt i VarP-> PatQ
pattern' = patQ . centre 

centre :: GridIx i => GridPatt i VarP-> GridPatt i VarP
centre grid = addAfter after bl $ addBefore before bl grid
    where bl = NonCursor PatternBlank

          before = coffset loc bounds
          after = roffset loc bounds
          
          loc = find isCursor grid
          bounds = size grid

          isCursor (NonCursor _) = False
          isCursor (Cursor _) = True

patQ :: GridIx i => GridPatt i VarP -> PatQ
patQ grid = gwrap tupP (gmap name grid)

-- We must convert to to GridPatt where the dimensionality is in the type
-- (above).
pattern :: GridPattern -> PatQ
pattern (GridPattern1D _ vs) = pattern' $ GridPatt1D (length vs) vs
pattern (GridPattern2D _ _ vs) = pattern' $ GridPatt2D (length vs) (length (head vs)) vs

-- Grid pattern with dimensionality in type and various helper functions.
-- Various ad-hoc polymorphic helper functions. These help us deal with both 1D
-- and 2D pattern simultaneously.
class (Ix i, Num i) => GridIx i where
    data GridPatt i :: * -> *
    gwrap :: ([a] -> a) -> GridPatt i a -> a 
    gmap :: (a -> b) -> GridPatt i a -> GridPatt i b
    addBefore :: i -> a -> GridPatt i a -> GridPatt i a
    addAfter :: i -> a -> GridPatt i a -> GridPatt i a
    find :: (a -> Bool) -> GridPatt i a -> i
    size :: GridPatt i a -> i

instance GridIx Int where
    data GridPatt Int a = GridPatt1D Int [a] deriving Show
    gwrap f (GridPatt1D _ l) = f l
    gmap f (GridPatt1D x l) = GridPatt1D x $ map f l
    addBefore i a (GridPatt1D x l) = GridPatt1D (x+i) $ (replicate i a) ++ l
    addAfter i a (GridPatt1D x l) = GridPatt1D (x+i) $ l ++ (replicate i a)
    find f (GridPatt1D _ xs) = fromJust (findIndex f xs)
    size (GridPatt1D x _) = x

instance GridIx (Int, Int) where
    data GridPatt (Int, Int) a = GridPatt2D Int Int [[a]] deriving Show
    gwrap f (GridPatt2D _ _ ll) = f (map f ll)
    gmap f (GridPatt2D x y ll) = GridPatt2D x y $ map (map f) ll
    addBefore _ _ grid = grid
    addAfter _ _ grid = grid
    find f grid = (1,1)
    size (GridPatt2D x y _) = (x, y)

--Allow arithmetics on tuples
tmap :: (a -> b) -> (a, a) -> (b, b)
tmap f (a, b) = (f a, f b)
tzip :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
tzip f (a, b) (c, d) = (f a c, f b d)

instance Num (Int, Int) where 
    (+) = tzip (+)  
    (*) = tzip (*)
    negate = tmap negate
    abs = tmap abs
    signum = tmap signum
    fromInteger i = tmap fromInteger (i, i)

--offset and range calculations--
-- | _ | @ | _ | _ | : uncentred grid
-- <--->   <------->
--   a       b-a-1
--
--      coffset   roffset
--       <--->     <--->
--
-- | _ | _ | @ | _ | _ | : centred grid
--
longest :: GridIx i => i -> i -> i
longest a b = max a (b-a-(fromInteger 1)) 
coffset :: GridIx i => i -> i -> i
coffset a b = (longest a b)- a
roffset :: GridIx i => i -> i -> i
roffset a b = (longest a b) + (fromInteger 1) - b + a

name :: VarP -> PatQ
name v = 
    case uncurse v of
      PatternVar x -> varP $ mkName x
      PatternBlank -> wildP
    
uncurse :: VarP -> VarP'
uncurse (Cursor v) = v
uncurse (NonCursor v) = v
