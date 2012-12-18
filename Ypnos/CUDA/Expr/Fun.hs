{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

fvar = varE . mkName
fcon = conE . mkName

--fun :: QuasiQuoter
--fun = QuasiQuoter { quoteExp = quoteExprExp,
--                   quotePat = quoteExprPat,
--                   quoteType = undefined,
--                   quoteDec = undefined
--                 }

--quoteExprExp :: String -> ExpQ
--quoteExprExp input = do loc <- location
--                        let pos = (loc_filename loc,
--                               fst (loc_start loc),
--                               snd (loc_start loc))
--                        expr <- (parseExpr gridFun) pos input
--                        dataToExpQ (const Nothing `extQ` interpret) expr


quoteExprPat :: String -> PatQ
quoteExprPat input = do loc <- location
                        let pos = (loc_filename loc,
                               fst (loc_start loc),
                               snd (loc_start loc))
                        expr <- (parseExpr gridFun) pos input
                        dataToPatQ (const Nothing) expr

--interpret :: GridFun -> Maybe (Q Exp)
--interpret (GridFun pattern body) = 
--    case parseExp body of
--      Left x -> error x
--      Right bodyExpr -> Just gridFun
--          where
--            gridFun = lamE [gpat] (return bodyExpr)
--            gpat = mkPattern pattern  

--mkPattern :: GridPattern -> PatQ
--mkPattern = intToPatt . centreCursor . ypToInt

data Intermediate i where 
    Inter :: Ix i => i -> Array i VarP -> Intermediate i

data family GridPattern' d 
data instance GridPattern' (Dim x) = 
    GridPattern1D' DimTag [VarP]
data instance GridPattern' (Dim x :* Dim y) = 
    GridPattern2D' DimTag DimTag [[VarP]]

ypToInt :: GridIx d i => GridPattern' d -> Intermediate i
ypToInt pat = 
        let range = getBounds pat 
            ls = safeConcat pat 
        in
        Inter (getCurIx pat) 
              (listArray range ls)

class (Ix i, Index d ~ i) => GridIx d i where
    getCurIx :: GridPattern' d -> i
    getBounds :: GridPattern' d -> (i, i)
    safeConcat :: GridPattern' d -> [VarP]

instance GridIx (Dim x) Int where
    getBounds (GridPattern1D' _ ls) = 
        (0::Int, length ls - 1)
    getCurIx (GridPattern1D' _ _) = 1
    safeConcat (GridPattern1D' _ ls) = ls

instance GridIx (Dim x :* Dim y) (Int,Int) where
    getBounds (GridPattern2D' _ _ ls) = 
        ((0::Int,0::Int), (length (head ls) -1, length ls -1))
    getCurIx (GridPattern2D' _ _ _) = (1,1)
    safeConcat (GridPattern2D' _ _ ls) = concat ls
    

--centreCursor :: Intermediate -> Intermediate
--intToPatt :: Intermediate -> PatQ

pkgPattern :: [VarP] -> PatQ
pkgPattern xs = tupP (mkVars xs)

mkVars :: [VarP] -> [PatQ]
mkVars vs = map getName vs

getName :: VarP -> PatQ
getName v = 
    case uncurse v of
      PatternVar x -> varP $ mkName x
      PatternBlank -> wildP
    
uncurse :: VarP -> VarP'
uncurse (Cursor v) = v
uncurse (NonCursor v) = v

getName' :: VarP' -> PatQ
getName' (PatternVar x) = varP $ mkName x
getName' PatternBlank = wildP
