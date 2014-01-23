{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Testing.Ypnos.CUDA.Expr.Fun where

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.List

import Ypnos.CUDA.Expr.Fun
import Ypnos.Expr.Expr

import Control.Monad

fun_tests = testGroup "Ypnos.CUDA.Expr.Fun"
    [ testGroup "Test correctly generated GridPatt"
      [ testProperty "Regularity" prop_regular
      , testProperty "Positive size" prop_pos_dim
      , testProperty "Has a cursor" prop_has_cursor ]
    , testProperty "The roffset is always positive" prop_pos_roff
    , testProperty "The coffset is always positive" prop_pos_coff
    , testProperty "Centring preserves regularity" prop_centre_preserve 
    , testProperty "The cursor is centred" prop_centre]


elementAt :: (Int,Int) -> (Int, Int) -> Gen VarP' -> Gen [[VarP]]
elementAt (x', y') (x, y) a = 
    sequence [ sequence [ element (x'',y'') 
        | y'' <- [0..(y-1)]] 
            | x'' <- [0..(x-1)]]
    where element (x, y) | x' == x && y' == y = liftM Cursor a
                         | otherwise = liftM NonCursor a

instance Arbitrary (GridPatt (Int, Int) VarP) where
    arbitrary = do 
        x <- choose (1, 10)
        y <- choose (1, 10)
        x' <- choose (0, x-1)
        y' <- choose (0, y-1)
        xs <- elementAt (x', y') (x, y) arbitrary
        return (GridPatt2D x y xs)

instance Arbitrary VarP where
    arbitrary = liftM NonCursor arbitrary

instance Arbitrary VarP' where
    arbitrary = oneof [ {-liftM PatternVar arbitrary-}
                      {-,-} return PatternBlank]

prop_regular :: GridPatt (Int, Int) VarP -> Bool
prop_regular (GridPatt2D x y xs) = all (\ i -> length i == y) xs
    && length xs == x

hasCursor xs = any cur (concat xs)
    where cur (Cursor _) = True
          cur (NonCursor _) = False
prop_has_cursor :: GridPatt (Int, Int) VarP -> Bool
prop_has_cursor (GridPatt2D _ _ xs) = hasCursor xs

prop_pos_dim :: GridPatt (Int, Int) VarP -> Bool
prop_pos_dim (GridPatt2D x y _) = x >= 0 && y >= 0

prop_centre_preserve :: GridPatt (Int, Int) VarP -> Bool
prop_centre_preserve grid = prop_regular (centre grid)

prop_pos_f :: (forall a. GridIx a => a -> a -> a) -> 
              GridPatt (Int, Int) VarP -> Bool 
prop_pos_f f grid = x >= 0 && y >= 0
    where (x, y) = f loc bounds
          loc = cursorLoc grid
          bounds = size grid

prop_pos_coff :: GridPatt (Int, Int) VarP -> Bool
prop_pos_coff grid = prop_pos_f coffset grid

prop_pos_roff :: GridPatt (Int, Int) VarP -> Bool
prop_pos_roff grid = prop_pos_f roffset grid

prop_centre :: GridPatt (Int, Int) VarP -> Bool
prop_centre grid = expected == actual
    where expected = (x `div` 2, y `div` 2)
          actual = cursorLoc grid'
          (x, y) = size grid'
          grid' = centre grid
