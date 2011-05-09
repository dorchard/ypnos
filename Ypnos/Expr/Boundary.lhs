> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeOperators #-}

> module Ypnos.Expr.Boundary where

> import Data.Generics
> import Language.Haskell.TH 
> import Language.Haskell.TH.Quote
> import Language.Haskell.SyntaxTrees.ExtsToTH
> import Ypnos.Expr.Expr

> import Ypnos.Core.Grid
> import Ypnos.Core.Combinators
> import Debug.Trace


> boundary :: QuasiQuoter
> boundary = QuasiQuoter { quoteExp = quoteExprExp,
>                     quotePat = undefined --,
>                     -- quoteType = undefined,
>                     -- quoteDec = undefined
>                   }

> quoteExprExp :: String -> Q Exp
> quoteExprExp input = do loc <- location
>                         let pos = (loc_filename loc,
>                                    fst (loc_start loc),
>                                    snd (loc_start loc))
>                         expr <- (parseExpr parseBoundaryDef) pos input
>                         dataToExpQ (const Nothing `extQ` interpret) expr


> interpret :: BoundaryDef -> Maybe (Q Exp)
> interpret (BoundaryDef elementType cases) = (show (BoundaryDef elementType cases)) `trace` interpretCases elementType cases

> interpretCases :: String -> [BoundaryCase] -> Maybe (Q Exp)
> interpretCases eT [] = Just $ [| NilB |]
> interpretCases eT (x:xs) = do x' <- interpretCase eT x
>                               xs' <- interpretCases eT xs
>                               return $ [| ConsB $(x') $(xs') |]

 interpretCase (Range i1 i2 exp) = 

> descriptorToDimensionality :: RegionDescriptor -> ([TyVarBndr],Cxt, Q Type)                               
> descriptorToDimensionality [x] = ([PlainTV $ mkName "d0"], [ClassP (mkName "DimIdentifier") [(VarT $ mkName "d0")]],
>                                                          appT (conT $ mkName "Dim") (varT $ mkName "d0"))
> descriptorToDimensionality xs = go xs 0
>                                   where
>                                    go [x] n = ([PlainTV $ mkName $ "d" ++ show n],
>                                                [ClassP (mkName "DimIdentifier") [(VarT $ mkName $ "d" ++ show n)]],
>                                                [t| Dim $(varT $ mkName $ "d" ++ show n) |])
>                                    go (x:xs) n = let (tvs, pred, typ) = go xs (n+1)
>                                                      dimN = appT (conT $ mkName "Dim") (varT $ mkName $ "d" ++ show n)
>                                                  in ([PlainTV $ mkName $ "d" ++ show n] ++ tvs,
>                                                      [ClassP (mkName "DimIdentifier")
>                                                         [(VarT $ mkName $ "d" ++ show n)]] ++ pred,
>                                                      [t| $(dimN) :* $(typ) |])
>                                                          
>                                                      
>                                  

> interpretCase elementType (Specific i exp) = 
>                 let (pat, typ) = interpretRegionDescriptor i                                     
>                 in case parseToTH exp of
>                       Left x -> error x
>                       Right expr -> Just fn 
>                          where 
>                            typ' = [t| BoundaryFun $(typ) $(conT $ mkName elementType) Static |]
>                            fn = sigE (appE (conE $ mkName "Static") (lamE [pat] (return $ expr))) typ'
> interpretCase elementType (Parameterised i var exp) = 
>                 let (pat, typ) = interpretRegionDescriptor i
>                 in case parseToTH exp of
>                      Left x -> ("blarg" ++ show x) `trace` error x
>                      Right expr -> Just fn
>                        where
>                          elemTypeConstr = conT $ mkName elementType
>                          (tvs, pred, dimTyp) = descriptorToDimensionality i
>                          typ' = [t| BoundaryFun $(typ) $(elemTypeConstr) 
>                                     (Dynamic (Grid $(dimTyp) (Nil, Static) $(elemTypeConstr))) |]
>                          typ'' = forallT tvs (return pred) typ'
>                          fn = sigE (appE (conE $ mkName "Dynamic") (lamE [tupP [pat, varP $ mkName var]]
>                                                                        (return expr))) typ''

> natPat :: Int -> (Q Pat, Q Type)
> natPat 0 = (conP (mkName "Zn") [], conT (mkName "Zn"))
> natPat n = if n < 0 then
>                let (par, typ) = natPat (-n)
>                in (conP (mkName "Neg") [par], appT (conT $ mkName "Neg") typ)
>            else 
>                let (par, typ) = natPat (n-1)
>                in (conP (mkName "S") [par], appT (conT $ mkName "S") typ)

> interpretSubRegion :: SubRegionDescriptor -> (Q Pat, Q Type)
> interpretSubRegion (Inner v) = (varP $ mkName v, conT $ mkName "Int")
> interpretSubRegion (Negative n) = let (pat, typ) = natPat (-n)
>                                   in (pat, appT (conT $ mkName "Nat") typ)
> interpretSubRegion (Positive n) = let (pat, typ) = natPat n
>                                   in (pat, appT (conT $ mkName "Nat") typ)

> interpretRegionDescriptor :: RegionDescriptor -> (Q Pat, Q Type)
> interpretRegionDescriptor regions = let (pats, types) = unzip $ map interpretSubRegion regions
>                                     in (tupP pats, foldl appT (tupleT (length types)) types)
