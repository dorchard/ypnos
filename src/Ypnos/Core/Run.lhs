> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE EmptyDataDecls #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE UndecidableInstances #-}

> module Ypnos.Core.Run where

> import Ypnos.Core.Grid
> import Ypnos.Core.Dimensions
> import Ypnos.Core.Boundary
> import Ypnos.Core.Types
> import Ypnos.Core.Dimensions

> import Data.Array.IArray
> import Data.Array.Unboxed
> import Data.Array.Base
> import qualified GHC.Arr as GHCArr

> import Data.Monoid
> import Data.List

> import Debug.Trace

Run stencil computations

> data Reduce r x = Reduce r x


> 
