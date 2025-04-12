{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module BenchHs where

import Data.Kind
import GHC.Base

type StrictList :: Type -> UnliftedType
data StrictList a = SNil | SCons !a !(StrictList a)

{-# OPAQUE  applyFunctionOne #-}
applyFunctionOne :: (Bool -> IO ()) -> StrictList Bool -> IO ()
applyFunctionOne f xs = go f xs
    where
        go _f SNil = pure ()
        go f (SCons x s) = f x >> go f s

type AppTen = (Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> (# #))

{-# OPAQUE  applyFunctionTen #-}
applyFunctionTen :: AppTen -> StrictList Bool -> IO Int
applyFunctionTen f xs = go f xs
    where
        go :: AppTen -> StrictList Bool -> IO Int
        go _f SNil = pure 42
        go f (SCons x s) = case f x x x x x x x x x x of !_ -> go f s

type AppThree = (Bool -> Bool -> Bool -> (# #))

{-# OPAQUE  applyFunctionThree #-}
applyFunctionThree :: AppThree -> StrictList Bool -> IO Int
applyFunctionThree f xs = go f xs
    where
        go :: AppThree -> StrictList Bool -> IO Int
        go _f SNil = pure 42
        go f (SCons x s) = case f x x x of !_ -> go f s
