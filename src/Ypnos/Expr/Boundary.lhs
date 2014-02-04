> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeOperators #-}

> module Ypnos.Expr.Boundary where

> import Data.Generics
> import Language.Haskell.TH 
> import Language.Haskell.TH.Syntax
> import Language.Haskell.TH.Quote
> import Language.Haskell.Meta.Parse
> import Ypnos.Expr.Expr

> import Ypnos.Core.Grid
> import Ypnos.Core.Types
> import Ypnos.Core.Combinators
> import Debug.Trace


> boundary :: QuasiQuoter
> boundary = QuasiQuoter { quoteExp = quoteExprExp,
>                          quotePat = undefined,
>                          quoteType = undefined,
>                          quoteDec = undefined
>                   }

> quoteExprExp :: String -> Q Exp
> quoteExprExp input = do loc <- location
>                         let pos = (loc_filename loc,
>                                    fst (loc_start loc),
>                                    snd (loc_start loc))
>                         expr <- (parseExpr parseBoundaryDef) pos input
>                         dataToExpQ (const Nothing `extQ` interpret) expr


> interpret :: BoundaryDef -> Maybe (Q Exp)
> interpret (BoundaryDef elementType cases) = interpretCases elementType cases

> appendInterpretedCases [] ys = ys
> appendInterpretedCases (x:xs) ys = [| $(conE $ mkName "ConsB") $(x) $(appendInterpretedCases xs ys) |]

> interpretCases :: String -> [BoundaryCase] -> Maybe (Q Exp)
> interpretCases eT [] = Just $ [| $(conE $ mkName "NilB") |]
> interpretCases eT (x:xs) = do let x' = interpretCase eT x
>                               xs' <- interpretCases eT xs
>                               return $ appendInterpretedCases x' xs'

 interpretCase (Range i1 i2 exp) = 

> descriptorToDimensionality :: RegionDescriptor -> ([TyVarBndr],Cxt, Q Type)                               
> descriptorToDimensionality [x] = ([PlainTV $ mkName "d0"], [ClassP (mkName "DimIdentifier") [(VarT $ mkName "d0")]],
>                                                          appT (conT $ mkName "Dim") (varT $ mkName "d0"))
> descriptorToDimensionality xs = go xs 0
>                                   where
>                                    go [x] n = ([PlainTV $ mkName $ "d" ++ show n],
>                                                [ClassP (mkName "DimIdentifier") [(VarT $ mkName $ "d" ++ show n)]],
>                                                [t| $(conT $ mkName "Dim") $(varT $ mkName $ "d" ++ show n) |])
>                                    go (x:xs) n = let (tvs, pred, typ) = go xs (n+1)
>                                                      dimN = appT (conT $ mkName "Dim") (varT $ mkName $ "d" ++ show n)
>                                                  in ([PlainTV $ mkName $ "d" ++ show n] ++ tvs,
>                                                      [ClassP (mkName "DimIdentifier")
>                                                         [(VarT $ mkName $ "d" ++ show n)]] ++ pred,
>                                                      (appT (appT (conT $ mkName ":*") dimN) typ))
>                                                          
>                                                      
> regionLTE (Negative x) (Positive y) = True
> regionLTE (Negative x) (Negative y) = x>=y
> regionLET (Positive x) (Negative y) = False
> regionLET (Positive x) (Positive y) = x<=y

> descriptorRange (Negative 0) (Positive x) = (Inner "a") : descriptorRange (Positive 1) (Positive x)
> descriptorRange (Negative x) (Positive y) = (Negative x) : descriptorRange (Negative (x-1)) (Positive y)
> descriptorRange (Positive x) (Positive y) = if (x<=y) then
>                                               (Positive x) : descriptorRange (Positive (x+1)) (Positive y)
>                                             else
>                                               []
> descriptorRange (Negative x) (Negative y) = if (x>=y) then 
>                                               (Negative x) : descriptorRange (Negative (x-1)) (Negative y)
>                                             else
>                                               [] 


> regionRange :: RegionDescriptor -> RegionDescriptor -> [RegionDescriptor]
> regionRange x y = (filter (\x -> not $ allInner x)) $ regionRange' x y
>                     where 
>                       allInner [] = True
>                       allInner (x:xs) = (x==(Inner "a")) && (allInner xs)
> regionRange' [x] [y] = map (\x -> [x]) $ descriptorRange x y
> regionRange' (x:xs) (y:ys) = concatMap (\x' -> map (\xs' -> x':xs') (regionRange' xs ys)) $ descriptorRange x y

                             
> interpretCase elementType (Range from to exp) = 
>   let wellFormed = map (\(a, b) -> 
>                      case (a, b) of 
>                          (Inner _, _) -> error "Boundary region must be from a numerical offset, e.g. -1, not *v"
>                          (_, Inner _) -> error "Boundary region must be from a numerical offset, e.g. -1, not *v"
>                          (x, y) -> if (regionLTE x y) then 
>                                       True
>                                    else
>                                       error "Syntax from x to y must have x less than y in all components") (zip from to)
>   in
>      concatMap (\r -> interpretCase elementType (Specific r exp)) $ regionRange from to
>                     
>     
> interpretCase elementType (Specific i exp) = 
>                 let (pat, typ) = interpretRegionDescriptor i                                     
>                 in case parseExp exp of
>                       Left x -> error x
>                       Right expr -> [fn]
>                          where 
>                            (tvs, pred, dimTyp) = descriptorToDimensionality i
>                            typ' = [t| $(conT $ mkName "BoundaryFun") $(varT $ mkName "g") $(dimTyp) $(typ) $(conT $ mkName elementType) $(conT $ mkName "Static") |]

>                            ctx = sequenceQ $ 
>                                   [--classP (mkName "IxConst") [varT $ mkName "g", dimTyp, typ, conT $ mkName elementType], 
>                                    classP (mkName "GridConstructor") [varT $ mkName "g"]
>                                    --classP (mkName "Size") [appT (varT $ mkName "g") dimTyp]
>                                   ] ++ (map return pred)
>                            typ'' = forallT ((PlainTV $ mkName "g"):tvs) ctx typ'

[t| Size ($(varT $ mkName "g") $(dimTyp)) |] typ'

(return ((ClassP (mkName "Size") [(varT $ mkName "g"), [t|$(dimTyp)|]]):pred)) typ'

>                            fn = sigE (appE (conE $ mkName "Static") (lamE [pat] (return $ expr))) typ''
> interpretCase elementType (Parameterised i var exp) = 
>                 let (pat, typ) = interpretRegionDescriptor i
>                 in case parseExp exp of
>                      Left x -> error x
>                      Right expr -> [fn]
>                        where
>                          elemTypeConstr = conT $ mkName elementType
>                          (tvs, pred, dimTyp) = descriptorToDimensionality i
>                          typ' = [t| $(conT $ mkName "BoundaryFun") $(varT $ mkName "g") $(dimTyp) $(typ) $(elemTypeConstr) 
>                                     $(conT $ mkName "Dynamic") |]
>                          ctx = sequenceQ $ 
>                                   [classP (mkName "IxConst") [varT $ mkName "g", dimTyp, [t| Nil |], conT $ mkName elementType], 
>                                    classP (mkName "GridConstructor") [varT $ mkName "g"],
>                                    classP (mkName "Size") [appT (varT $ mkName "g") dimTyp]] ++ (map return pred)

>                          typ'' = forallT ((PlainTV $ mkName "g"):tvs) ctx typ'
>                          fn = sigE (appE (conE $ mkName "Dynamic") (lamE [tupP [pat, varP $ mkName var]]
>                                                                        (return expr))) typ''

> natPat :: Int -> (Q Pat, Q Type)
> natPat 0 = (conP (mkName "Zn") [], conT (mkName "Zn"))
> natPat n = let (par, typ) = natPat (n-1)
>            in (conP (mkName "S") [par], appT (conT $ mkName "S") typ)

> intPat :: Int -> (Q Pat, Q Type)
> intPat n = if n < 0 then
>                let (par, typ) = natPat (-n)
>                in (conP (mkName "Neg") [par], appT (conT $ mkName "IntT") (appT (conT $ mkName "Neg") typ))
>            else 
>                let (par, typ) = natPat n
>                in (conP (mkName "Pos") [par], appT (conT $ mkName "IntT") (appT (conT $ mkName "Pos") typ))



> interpretSubRegion :: SubRegionDescriptor -> (Q Pat, Q Type)
> interpretSubRegion (Inner v) = (varP $ mkName v, conT $ mkName "Int")
> interpretSubRegion (Negative n) = intPat (-n)
> interpretSubRegion (Positive n) = intPat n

> interpretRegionDescriptor :: RegionDescriptor -> (Q Pat, Q Type)
> interpretRegionDescriptor regions = let (pats, types) = unzip $ map interpretSubRegion regions
>                                     in (tupP pats, foldl appT (tupleT (length types)) types)

> fcon c = conE $ mkName c