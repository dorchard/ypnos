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
mkPattern pat = 
    case pat of
        (GridPattern1D _ _) -> mkPattern1D pat
        (GridPattern2D _ _ vars) -> tupP (map pkgPattern vars)
    
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

mkPattern1D :: GridPattern -> PatQ
mkPattern1D pat = tupP (mkPattern1D' (fromJust (getCursorLocation pat [0])) (getNonCursorLocations pat [0]) 0)

mkPattern1D' :: Position -> [Position] -> Int -> [PatQ]
mkPattern1D' p (n:ncs) x | l==x = (getName' c):(mkPattern1D' p (n:ncs) (x+1))
                      | ln==x = (getName' nc):(mkPattern1D' p ncs (x+1))
                      | x > 2*l = []
                      | otherwise = wildP:(mkPattern1D' p ncs (x+1)) --TODO: avoid this repetition
    where Position nc (ln:_) = n 
          Position c (l:_) = p
mkPattern1D' p _ x = [] --TODO: deal with case of cursor being last element

getName' :: VarP' -> PatQ
getName' (PatternVar x) = varP $ mkName x
getName' PatternBlank = wildP

data Position = Position VarP' [Int] deriving Show

getCursorLocation :: GridPattern -> [Int] -> Maybe Position
getCursorLocation (GridPattern1D x ((Cursor v):vs)) loc = Just (Position v loc)
getCursorLocation (GridPattern1D x ((NonCursor _):vs)) (l:ls) = getCursorLocation (GridPattern1D x vs) ((l+1):ls)
getCursorLocation (GridPattern1D x []) _ = Nothing
getCursorLocation (GridPattern2D x y (v:vs)) (l:ls) = 
                case getCursorLocation (GridPattern1D x v) (0:l:ls) of
                    Just a -> Just a
                    Nothing -> getCursorLocation (GridPattern2D x y vs) ((l+1):ls)
getCursorLocation (GridPattern2D x y []) _ = Nothing

getNonCursorLocations (GridPattern1D x curs) ls = 
        case curs of
            (v:vs) ->
                let next = getNonCursorLocations (GridPattern1D x vs) ((l+1):locs)
                    (l:locs) = ls
                in case v of
                    (Cursor _) -> next
                    (NonCursor v) -> (Position v ls):next
            [] -> []
getNonCursorLocations (GridPattern2D x y curs) ls =
        case curs of
            (v:vs) ->
                let current = getNonCursorLocations (GridPattern1D x v) (0:ls)
                    (l:locs) = ls
                    next = getNonCursorLocations (GridPattern2D x y vs) (l+1:locs)
                in current ++ next
            [] -> []

getSize :: GridPattern -> [Int]
getSize (GridPattern1D _ vs) = [length vs]
getSize (GridPattern2D x _ (v:vs)) = (length (v:vs)):(getSize (GridPattern1D x v)) 
