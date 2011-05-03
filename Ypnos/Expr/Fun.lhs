> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE TemplateHaskell #-}

> module Ypnos.Expr.Fun where

> import Data.Maybe
> import Data.Generics
> import Language.Haskell.TH 
> import Language.Haskell.TH.Quote
> import Language.Haskell.SyntaxTrees.ExtsToTH
> import Ypnos.Expr.Expr

> fvar = varE . mkName
> fcon = conE . mkName

> fun :: QuasiQuoter
> fun = QuasiQuoter { quoteExp = quoteExprExp,
>                     quotePat = quoteExprPat --,
>                     --quoteType = undefined,
>                     --quoteDec = undefined
>                   }

> quoteExprExp :: String -> ExpQ
> quoteExprExp input = do loc <- location
>                         let pos = (loc_filename loc,
>                                fst (loc_start loc),
>                                snd (loc_start loc))
>                         expr <- (parseExpr gridFun) pos input
>                         dataToExpQ ((const Nothing) `extQ` interpret) expr
> 

> quoteExprPat :: String -> PatQ
> quoteExprPat input = do loc <- location
>                         let pos = (loc_filename loc,
>                                fst (loc_start loc),
>                                snd (loc_start loc))
>                         expr <- (parseExpr gridFun) pos input
>                         dataToPatQ (const Nothing) expr

> interpret :: GridFun -> Maybe (Q Exp)
> interpret (GridFun pattern body) = 
>     case parseToTH body of
>       Left x -> error x
>       Right bodyExpr -> Just gridFun
>           where
>             gridFun = tupE [lamE [gpat] (letE vals' (monadicLetBind vars bodyExpr)), dim]
> 
>             vars = getVars pattern             
  
>             toListE [] = []
>             toListE ((l, r):ss) = (tupE [litE $ integerL l,
>                                             litE $ integerL r]):(toListE ss)

>             gpat = varP $ mkName "reserved_grid"
             
>             vals' = witness:(cursor:vals)

>             cursor = valD (varP $ mkName "reserved_i")
>                               (normalB $ [| $(fvar "cursor") -- $(fvar "gridConstr") 
>                                           $(fvar "reserved_grid") |]) []
>             witness = valD wildP
>                               (normalB $ [| $(fvar "witness") $(fvar "funGridWitness") 
>                                             $(fvar "reserved_grid") |]) []
>
>             (vals, dim) = case (interpretPattern pattern) of
>                 (Nothing, dim) -> ([], dim)
>                 (Just p, dim) -> (p, dim)

-- creates an indexing expression from either a pair index or a single value

> monadicLetBind :: [String] -> Exp -> Q Exp
> monadicLetBind [] b = [| $(fvar "unit") $(return b) |]
> monadicLetBind (v:vs) b = [| $(fvar "bind") $(fvar (v ++ "M")) $(lamBind v (monadicLetBind vs b)) |]

> lamBind v e = lamE [varP $ mkName v] e

> getVars :: GridPattern -> [String]
> getVars (GridPattern1D _ vps) = catMaybes (map varPtoString vps)
> getVars (GridPattern2D _ _ vps) = catMaybes (concat (map (map varPtoString) vps))

> varPtoString :: VarP -> Maybe String
> varPtoString (Cursor (PatternVar s)) = Just s
> varPtoString (Cursor _) = Nothing
> varPtoString (NonCursor (PatternVar s)) = Just s
> varPtoString (NonCursor _) = Nothing

> data Indexers = OneD Integer | TwoD (Integer, Integer)

> indexExpr (TwoD (0, 0)) = [| $(fvar "reserved_i") |]
>                                     
> indexExpr (TwoD (a, b)) = [| $(fvar "reserved_i") `addIx` (StepIx (fromInteger a) (StepIx (fromInteger b) NilIx))|]

> indexExpr (OneD 0) = [| $(fvar "reserved_i") |]
> indexExpr (OneD a) = [| $(fvar "reserved_i") `addIx` ((StepIx (fromInteger a) NilIx)) |]

> toDec ((PatternBlank, _)) = Nothing
> toDec ((PatternVar x), i) = Just $ valD (varP $ mkName $ x++"M")
>                                  (normalB $ [| $(fvar "offset") -- $(fvar "gridConstr") 
>                                                $(fvar "reserved_grid") $(indexExpr i) |]) []
> toDecs = catMaybes . map toDec

> toIndex1D s n [] = []
> toIndex1D s n (x:xs) = (OneD $ s*n):(toIndex1D s (n+1) xs)

> toIndex2D _ _ [] = []
> toIndex2D (s1, s2) (n1, n2) (x:xs) = (TwoD (s1*n1, s2*n2)):(toIndex2D (s1, s2) (n1, n2+1) xs)

> toGrid1DLets l c r = 
>     (toDecs (zip l (toIndex1D (-1) 1 l))) ++ (toDecs [(c, (OneD 0))]) ++ (toDecs (zip r (toIndex1D 1 1 r)))

--    | ((length vars)/=1) && ((count vars)==0) = fail $ "A central cusor point @ must be given if more than one variable is to be matched"
--    | (count vars)>1 = fail $ "Only one cursor point @ can be given"
--    | otherwise = 

> toGrid2DLets l c r = 
>     (toGrid2DVLets l (-1) 1)++(toGrid2DVLets r 1 1)++(toGrid2DVLets c 0 1)
>     where
>        toGrid2DVLets [] s n = []
>        toGrid2DVLets ((t,c',b):ys) s n = 
>            let
>                top = toDecs (zip t (toIndex2D (s, -1) (n, 1) t))
>                centre = toDecs (zip [c'] (toIndex2D (s, 0) (n, 0) [c']))
>                bottom = toDecs (zip b (toIndex2D (s, 1) (n, 1) b))
>            in
>                (top++centre++bottom)++(toGrid2DVLets ys s (n+1))

> interpretPattern :: GridPattern -> (Maybe [DecQ], ExpQ)
> interpretPattern (GridPattern1D dim vars) = 
> 
>     (Just $ toGrid1DLets l c r, dimensions)
>         where
>           (l, c, r) = split1D vars []
>           xb1 = length l
>           xb2 = length r
>           dimensions = [| (StepIx xb1 NilIx, StepIx xb2 NilIx) |]
>                                                                  

> interpretPattern (GridPattern2D dim dim2 vars) = 
>     
>     (Just $ toGrid2DLets lefts [(cl, cc, cr)] rights, dimensions)
>          where
>          (l, c, r) = split2D vars []
>          (cl, cc, cr) = split1D c []
 
          cursorSplit = valD (tupP [varP $ mkName "reserved_x", varP $ mkName "reserved_y"])
                        (normalB $ (varE $ mkName "reserved_i")) []

>          lefts = map (split1Drelative (length cl)) l
>          rights = map (split1Drelative (length cl)) r

>          maxl = max (maximum (0:(map (\(x, _, _) -> length x) (lefts++rights)))) (length cl)
>          maxr = max (maximum (0:(map (\(_, _, x) -> length x) (lefts++rights)))) (length cr)
           
>          xb1 = length l
>          xb2 = length r
>          dimensions = [| (StepIx maxl (StepIx maxr NilIx),
>                           StepIx xb1 (StepIx xb2 NilIx)) |]

--interpretPattern _ = (Nothing, [(0, 0)])

-- Split a 1D pattern match into left, center, and right components
-- where the center is the first seen cursor

> remCursor (Cursor x) = x
> remCursor (NonCursor x) = x

> split1D ::[VarP] -> [VarP'] -> ([VarP'], VarP', [VarP'])
> split1D [Cursor x] l = (l, x, [])
> split1D [NonCursor x] l = (l, x, [])
> split1D ((Cursor x):xs) l = (l, x, (map remCursor xs))
> split1D ((NonCursor x):xs) l = split1D xs (x:l)

-- Split a 1D pattern match relative to a cursor already seen
-- ignore all other cursors
-- 

> split1Drelative :: Int -> [VarP] -> ([VarP'], VarP', [VarP'])
> split1Drelative = split1Drelative' [] 0

> split1Drelative' :: [VarP'] -> Int -> Int -> [VarP] -> ([VarP'], VarP', [VarP'])
> split1Drelative' left c n (x:xs)
>     | c<n = split1Drelative' ((remCursor x):left) (c+1) n xs
>     | otherwise = (left, remCursor x, (map remCursor xs))

-- Split a 2D pattern match into left, center, and right row components
-- where the center is the first row with a cursor (using count to check)

> split2D :: [[VarP]] -> [[VarP]] -> ([[VarP]], [VarP], [[VarP]])
> split2D [x] l = (l, x, [])
> split2D (x:xs) l = if (count x)>0 then (l, x, xs)
>                    else split2D xs (x:l)


> count :: [VarP] -> Integer
> count [] = 0
> count ((Cursor _):xs) = 1+(count xs)
> count (_:xs) = count xs
 
