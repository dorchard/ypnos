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
centre = id

patQ :: GridIx i => GridPatt i VarP -> PatQ
patQ grid = gwrap tupP (gmap name grid)

-- We must convert to to GridPatt where the dimensionality is in the type
-- (above).
pattern :: GridPattern -> PatQ
pattern (GridPattern1D _ vs) = pattern' $ GridPatt1D vs
pattern (GridPattern2D _ _ vs) = pattern' $ GridPatt2D vs

-- Grid pattern with dimensionality in type and various helper functions.
-- Various ad-hoc polymorphic helper functions. These help us deal with both 1D
-- and 2D pattern simultaneously.
class (Ix i, Num i) => GridIx i where
    data GridPatt i :: * -> *
    gwrap :: ([a] -> a) -> GridPatt i a -> a 
    gmap :: (a -> b) -> GridPatt i a -> GridPatt i b

instance GridIx Int where
    data GridPatt Int a = GridPatt1D [a] deriving Show
    gwrap f (GridPatt1D l) = f l
    gmap f (GridPatt1D l) = GridPatt1D $ map f l

instance GridIx (Int, Int) where
    data GridPatt (Int, Int) a = GridPatt2D [[a]] deriving Show
    gwrap f (GridPatt2D ll) = f (map f ll)
    gmap f (GridPatt2D ll) = GridPatt2D $ map (map f) ll

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
--      coffset
--       <--->
--
-- | _ | _ | @ | _ | _ | : centred grid
-- 
-- <------------------->
--         crange
longest :: GridIx i => i -> i -> i
longest a b = max a (b-a-(fromInteger 1)) 
coffset :: GridIx i => i -> i -> i
coffset a b = (longest a b)- a
crange :: GridIx i => i -> i -> (i,i)
crange a b = (fromInteger 0, (fromInteger 2)*(longest a b) + (fromInteger 1))

name :: VarP -> PatQ
name v = 
    case uncurse v of
      PatternVar x -> varP $ mkName x
      PatternBlank -> wildP
    
uncurse :: VarP -> VarP'
uncurse (Cursor v) = v
uncurse (NonCursor v) = v
