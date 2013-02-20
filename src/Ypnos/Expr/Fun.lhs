> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE ImplicitParams #-}

> module Ypnos.Expr.Fun where

> import Ypnos.Core.Grid

> import Data.Maybe
> import Data.Generics
> import Language.Haskell.TH 
> import Language.Haskell.TH.Quote
> import Language.Haskell.Meta.Parse
> import Ypnos.Expr.Expr

> import Debug.Trace

> fvar = varE . mkName
> fcon = conE . mkName

> funCPU :: QuasiQuoter
> funCPU = let ?safe = True 
>       in QuasiQuoter { quoteExp = quoteExprExp,
>                        quotePat = quoteExprPat --,
>                        --quoteType = undefined,
>                        --quoteDec = undefined
>                      }

> quoteExprExp :: (?safe :: Bool) => String -> ExpQ
> quoteExprExp input = do loc <- location
>                         let pos = (loc_filename loc,
>                                fst (loc_start loc),
>                                snd (loc_start loc))
>                         expr <- (parseExpr gridFun) pos input
>                         dataToExpQ ((const Nothing) `extQ` interpret) expr
> 

> quoteExprPat :: (?safe :: Bool) => String -> PatQ
> quoteExprPat input = do loc <- location
>                         let pos = (loc_filename loc,
>                                fst (loc_start loc),
>                                snd (loc_start loc))
>                         expr <- (parseExpr gridFun) pos input
>                         dataToPatQ (const Nothing) expr

> interpret :: (?safe :: Bool) => GridFun -> Maybe (Q Exp)
> interpret (GridFun pattern body) = 
>     case parseExp body of
>       Left x -> error x
>       Right bodyExpr -> Just gridFun
>           where
>             gridFun = lamE [gpat] (letE bindings (return bodyExpr))
>             gpat = varP $ mkName "reserved_grid"  
>             bindings = interpretPattern pattern


> natToNatExp :: Int -> ExpQ
> natToNatExp 0 = [| $(conE $ mkName "Zn") |]
> natToNatExp n = [| $(conE $ mkName "S") $(natToNatExp (n-1)) |]

> intToIntExp :: Int -> ExpQ
> intToIntExp n = if n < 0 then
>                   [| $(conE $ mkName "Neg") $(natToNatExp (-n)) |]
>                 else
>                   [| $(conE $ mkName "Pos") $(natToNatExp n) |]
>                 

> mkLetBind1D v x = [valD (varP $ mkName v) (normalB [| $(fvar "index1D") $(intToIntExp $ x) x $(fvar "reserved_grid")|]) []]

 [valD (varP $ mkName v) (normalB [| $(fvar "unsafeIndex1D") x $(fvar "reserved_grid") |]) [], valD (wildP) (normalB e) []]

> mkLetBind2D v x y = [valD (varP $ mkName v) (normalB
>                       [| $(fvar (if ?safe then "index2D" else "unsafeIndex2D"))
>                          ($(intToIntExp $ x), $(intToIntExp $ y)) (x, y)
>                          $(fvar "reserved_grid")|]) []] 

 [valD (varP $ mkName v) (normalB [| $(fvar "unsafeIndex2D") (x, y) $(fvar "reserved_grid") |]) [], valD (wildP) (normalB e) []]

> toIndex1D s n [] = []
> toIndex1D s n ((PatternBlank):xs) = toIndex1D s (n+1) xs
> toIndex1D s n ((PatternVar v):xs) = (mkLetBind1D v (s*n))
>                                     ++ (toIndex1D s (n+1) xs)

> toIndex2D _ _ [] = []
> toIndex2D (s1, s2) (n1, n2) ((PatternBlank):xs) = toIndex2D (s1, s2) (n1, n2+1) xs
> toIndex2D (s1, s2) (n1, n2) ((PatternVar v):xs) = (mkLetBind2D v (s1*n1) (s2*n2))
>                                                   ++(toIndex2D (s1, s2) (n1, n2+1) xs)

> toGrid1DLets l c r = 
>    (toIndex1D (-1) 1 l) ++ (toIndex1D 1 1 r) ++
>       (case c of
>          PatternVar x -> [valD (varP $ mkName x) (normalB [|$(fvar "indexC") $(fvar "reserved_grid")|]) []]
>          _ -> [])

> toGrid2DLets l c r = 
>     (toGrid2DVLets l (-1) 1)++(toGrid2DVLets c 0 1)++(toGrid2DVLets r 1 1)
>     where
>        toGrid2DVLets [] s n = []
>        toGrid2DVLets ((t,c',b):ys) s n = 
>            let
>                top = toIndex2D (s, -1) (n, 1) t
>                centre = toIndex2D (s, 0) (n, 1) [c']
>                         -- Could use indexC for the cursor pattern, but not really necessary
>                         -- if (s==0) then 
>                         --     case c' of
>                         --        (PatternVar x) -> [mkLetBind x [|$(fvar "indexC") $(fvar "reserved_grid")|]]
>                         --        _ -> []
>                         -- else
>                         --     toIndex2D (s, 0) (n, 1) [c']
>                bottom = toIndex2D (s, 1) (n, 1) b
>            in
>                (top++centre++bottom)++(toGrid2DVLets ys s (n+1))

> interpretPattern :: (?safe :: Bool) => GridPattern -> [DecQ]
> interpretPattern (GridPattern1D dim vars) = 
>       toGrid1DLets l c r
>         where
>           (l, c, r) = split1D vars []

> interpretPattern (GridPattern2D dim dim2 vars) = 
>       toGrid2DLets lefts [(cl, cc, cr)] rights
>          where
>          (l, c, r) = split2D vars []
>          (cl, cc, cr) = split1D c []
  
>          lefts = map (split1Drelative (length cl)) l
>          rights = map (split1Drelative (length cl)) r

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
 
