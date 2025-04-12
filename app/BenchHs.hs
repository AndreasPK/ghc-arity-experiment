{-# LANGUAGE UnliftedDatatypes #-}

module BenchHs where

import Data.Kind
import GHC.Base

type StrictList :: Type -> UnliftedType
data StrictList a = SNil | SCons !a !(StrictList a)

{-# OPAQUE  applyFunction #-}
applyFunction :: (Bool -> IO ()) -> StrictList Bool -> IO ()
applyFunction f xs = go f xs
    where
        go _f SNil = pure ()
        go f (SCons x s) = f x >> go f s

-- {-# NOINLINE mapBools #-}
-- mapBools :: (Bool -> Bool) -> [Bool] -> [Bool]
-- mapBools f xs = myMap f xs

-- myMap _f [] = []
-- myMap f (x:s) =
--     let !s' = myMap f s
--         !x' = f x
--     in x' : s'