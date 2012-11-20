{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Ypnos.CUDA.Expr.Fun where

import Ypnos.Core.Grid

import Data.Maybe
import Data.Generics
import Language.Haskell.TH 
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
                        dataToExpQ (const Nothing) expr


quoteExprPat :: String -> PatQ
quoteExprPat input = do loc <- location
                        let pos = (loc_filename loc,
                               fst (loc_start loc),
                               snd (loc_start loc))
                        expr <- (parseExpr gridFun) pos input
                        dataToPatQ (const Nothing) expr
