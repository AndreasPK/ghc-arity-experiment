{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Criterion.Main

import Unsafe.Coerce

import GHC.Exts
import BenchHs
import GHC.IO.Unsafe (unsafePerformIO)
import Data.Array.Byte (MutableByteArray)
import Data.Primitive (newByteArray)

foreign import prim "applyFunctionHsLike"
    applyFunctionHsLike_prim :: Any -> Any -> Int#

foreign import prim "applyFunctionDirect"
    applyFunctionDirect_prim :: Any -> Any -> Int#

foreign import prim "applyFunctionDirectThree"
    applyFunctionDirectThree_prim :: Any -> Any -> Int#

foreign import prim "applyFunctionDirectTen"
    applyFunctionDirectTen_prim :: Any
                                -> Any
                                -> Int#

applyFunctionHsLike :: (Bool -> IO ()) -> StrictList Bool -> IO Int
applyFunctionHsLike f xs = pure $ I# ((unsafeCoerce applyFunctionHsLike_prim) f xs)

applyFunctionDirect :: (Bool -> IO ()) -> StrictList Bool -> IO Int
applyFunctionDirect f xs = pure $ I# ((unsafeCoerce applyFunctionDirect_prim) f xs)

applyFunctionDirectTen :: AppTen
                       -> StrictList Bool -> IO Int
applyFunctionDirectTen f xs = do
    case (unsafeCoerce applyFunctionDirectTen_prim :: AppTen -> StrictList Bool -> Int#) f xs of
        n -> pure (I# n)

applyFunctionDirectThree :: AppThree
                       -> StrictList Bool -> IO Int
applyFunctionDirectThree f xs = do
    case (unsafeCoerce applyFunctionDirectThree_prim :: AppThree -> StrictList Bool -> Int#) f xs of
        n -> pure (I# n)

toSList :: [a] -> StrictList a
toSList [] = SNil
toSList (x:s) = SCons x (toSList s)

ba :: MutableByteArray RealWorld
ba = unsafePerformIO $ newByteArray 10000

{-# NOINLINE bools #-}
bools :: [Bool]
bools = replicate 2000 False

{-# NOINLINE eval_ #-}
eval_ :: a -> IO ()
eval_ !_ = pure ()

{-# OPAQUE evalThree_ #-}
evalThree_ :: Bool -> Bool -> Bool -> (# #)
evalThree_ !_ !_ !_ = (# #)

{-# OPAQUE evalTen_ #-}
evalTen_ :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> (# #)
evalTen_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_  = (# #)

-- Our benchmark harness.
main :: IO ()
main = do
    let !sbools = toSList bools
    defaultMain [
        bgroup "apply - Arity 1 Matched"
                [ bench "hs_apply"          $ nfIO (applyFunctionOne eval_ sbools)
                , bench "cmm_direct"        $ nfIO (applyFunctionDirect eval_ sbools)
                , bench "cmm_hslike"        $ nfIO (applyFunctionHsLike eval_ sbools)
                -- , bench "5"  $ nf id fastMapBools
                ],
        bgroup "apply - Arity 3 Matched"
                [ bench "hs_apply"          $ nfIO (applyFunctionThree evalThree_ sbools)
                , bench "cmm_direct"        $ nfIO (applyFunctionDirectThree evalThree_ sbools)
                -- , bench "cmm_hslike"        $ nfIO (applyFunctionHsLike eval_ sbools)
                -- , bench "5"  $ nf id fastMapBools
                ],
        bgroup "apply - Arity 10 Matched"
                [ bench "hs_apply"          $ nfIO (applyFunctionTen evalTen_ sbools)
                , bench "cmm_direct"        $ nfIO (applyFunctionDirectTen evalTen_ sbools)
                -- , bench "cmm_hslike"        $ nfIO (applyFunctionHsLike eval_ sbools)
                -- , bench "5"  $ nf id fastMapBools
                ]
        ]