{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Ypnos.CUDA.Expr.Fun where

import Ypnos.Core.Grid

import Data.Maybe
import Data.Generics
import Language.Haskell.TH 
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Ypnos.Expr.Expr

import Debug.Trace

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

mkPattern :: GridPattern -> PatQ
mkPattern (GridPattern1D dim vars) =
    tupP pats
    where 
    pats = mkVars vars

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
