{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

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

-- |The 'QuasiQuoter' that converts Ypnos syntax into Haskell code.
funGPU :: QuasiQuoter
funGPU = QuasiQuoter { quoteExp = quoteExprExp,
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

-- |When we eventually run this is what gets evaluated.
interpret :: GridFun -> Maybe (Q Exp)
interpret (GridFun pat body) =
   case parseExp body of
     Left x -> error x
     Right bodyExpr -> Just gridFun
         where
           gridFun = lamE [gpat] (return bodyExpr)
           gpat = pattern pat

-- * Pattern Conversion
-- |Before converting into the patq representation we must first centre the
-- pattern for accelerate.
pattern' :: GridIx i => GridPatt i VarP-> PatQ
pattern' = patQ . centre

-- |We must convert to to GridPatt where the dimensionality is in the type
-- (above).
pattern :: GridPattern -> PatQ
pattern (GridPattern1D _ vs) = pattern' $ GridPatt1D (length vs) vs
pattern (GridPattern2D _ _ vs) = pattern' $ GridPatt2D (length vs) (length (head vs)) vs

patQ :: GridIx i => GridPatt i VarP -> PatQ
patQ grid = gwrap tupP (gmap name grid)

-- * Centering the Pattern
-- |Pads the pattern to put the cursor in the middle.
-- An example uncentred grid:
--
-- @
--          b
--  *---------------*
--  | _ | c | _ | _ |
--  *---*   *-------*
--    a       b-a-1
-- @
--
-- After the centrering:
--
-- @
--         coffset roffset
--          *---*   *---*
--  | _ | _ | c | _ | _ |
-- @
centre :: GridIx i => GridPatt i VarP-> GridPatt i VarP
centre grid = addAfter after bl $ addBefore before bl grid
    where bl = NonCursor PatternBlank

          before = coffset loc bounds
          after = roffset loc bounds

          loc = cursorLoc grid
          bounds = size grid

-- ** Helpers for centering
-- | Finds the maximum of a and b-a-1.
longest :: GridIx i => i -> i -> i
longest a b = elMax a (b-a-(fromInteger 1))

-- | The cursor offset
coffset :: GridIx i => i -> i -> i
coffset a b = (longest a b)- a

-- | The range offset ie. how much we must grow the grid.
roffset :: GridIx i => i -> i -> i
roffset a b = (longest a b) + (fromInteger 1) - b + a

cursorLoc :: GridIx i => GridPatt i VarP -> i
cursorLoc grid = find isCursor grid
    where isCursor (NonCursor _) = False
          isCursor (Cursor _) = True

-- |Grid pattern with dimensionality in type and various helper functions.
-- Various ad-hoc polymorphic helper functions. These help us deal with both 1D
-- and 2D pattern simultaneously.
class (Ix i, Num i, ElMax i) => GridIx i where
    data GridPatt i :: * -> *
    gwrap :: ([a] -> a) -> GridPatt i a -> a
    gmap :: (a -> b) -> GridPatt i a -> GridPatt i b
    addBefore :: i -> a -> GridPatt i a -> GridPatt i a
    addAfter :: i -> a -> GridPatt i a -> GridPatt i a
    find :: (a -> Bool) -> GridPatt i a -> i
    size :: GridPatt i a -> i

-- |1D Case instanciated with 'GridPatt1D'
instance GridIx Int where
    data GridPatt Int a =
        -- | Creates a 1D grid.
        GridPatt1D Int [a] deriving Show
    gwrap f (GridPatt1D _ l) = f l
    gmap f (GridPatt1D x l) = GridPatt1D x $ map f l
    addBefore i a (GridPatt1D x l) = GridPatt1D (x+i) $ (replicate i a) ++ l
    addAfter i a (GridPatt1D x l) = GridPatt1D (x+i) $ l ++ (replicate i a)
    find f (GridPatt1D _ xs) = fromJust (findIndex f xs)
    size (GridPatt1D x _) = x

-- |Helper function for adding before or after in a 2D grid.
addBeforeAfter ::  (forall b . [b] -> [b] -> [b])
                -> (Int,Int) -> a -> GridPatt (Int,Int) a
                -> GridPatt (Int,Int) a
addBeforeAfter f (i,j) a (GridPatt2D x y ll) =
    GridPatt2D (x+i) (y+j) (topbottom (map leftright ll))
    where topbottom = f $ replicate (i) (replicate (y+j) a)
          leftright = f $ replicate j a

-- |2D Case instanciated with 'GridPatt2D'
instance GridIx (Int, Int) where
    data GridPatt (Int, Int) a =
        -- | Creates a 2D grid.
        GridPatt2D Int Int [[a]] deriving Show
    gwrap f (GridPatt2D _ _ ll) = f (map f ll)
    gmap f (GridPatt2D x y ll) = GridPatt2D x y $ map (map f) ll
    addBefore = addBeforeAfter (\ x y -> x ++ y)
    addAfter = addBeforeAfter (\ x y -> y ++ x)
    find f (GridPatt2D _ _ xs) = (i, j)
        where i = fromJust (findIndex isJust indexs)
              j = fromJust (indexs !! i)
              indexs = map (findIndex f) xs
    size (GridPatt2D x y _) = (x, y)

-- ** Helpers for Offset and Range calculations

instance Num (Int, Int) where
    (+) = tzip (+)
    (*) = tzip (*)
    negate = tmap negate
    abs = tmap abs
    signum = tmap signum
    fromInteger i = tmap fromInteger (i, i)

tmap :: (a -> b) -> (a, a) -> (b, b)
tmap f (a, b) = (f a, f b)
tzip :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
tzip f (a, b) (c, d) = (f a c, f b d)

class ElMax i where
    -- |Give an element wise maximum of the arguments.
    elMax :: i -> i -> i

instance ElMax Int where
    elMax = max

instance ElMax (Int, Int) where
    elMax = tzip max


-- ** Converting the basic elements.
-- Handles conversion between 'VarP' and 'PatQ'.
name :: VarP -> PatQ
name v =
    case uncurse v of
      PatternVar x -> varP $ mkName x
      PatternBlank -> wildP

-- | Strips 'Cursor' or 'NonCursor'
uncurse :: VarP -> VarP'
uncurse (Cursor v) = v
uncurse (NonCursor v) = v
