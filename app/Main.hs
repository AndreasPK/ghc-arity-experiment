{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}

module Main where

import Criterion.Main
import Control.DeepSeq

import Unsafe.Coerce

import GHC.Exts
import BenchHs
import Numeric

foreign import prim "applyFunctionHsLike"
    applyFunctionHsLike_prim :: Any -> Any -> Int#

foreign import prim "applyFunctionDirect"
    applyFunctionDirect_prim :: Any -> Any -> Int#

applyFunctionHsLike :: (Bool -> IO ()) -> StrictList Bool -> IO Int
applyFunctionHsLike f xs = pure $ I# ((unsafeCoerce applyFunctionHsLike_prim) f xs)

applyFunctionDirect :: (Bool -> IO ()) -> StrictList Bool -> IO Int
applyFunctionDirect f xs = pure $ I# ((unsafeCoerce applyFunctionDirect_prim) f xs)

toSList [] = SNil
toSList (x:s) = SCons x (toSList s)

{-# NOINLINE bools #-}
bools :: [Bool]
bools = replicate 100000 False

{-# NOINLINE eval_ #-}
eval_ :: a -> IO ()
eval_ !_x = pure ()
eval_ !_x = print "Hi" -- pure ()

-- Our benchmark harness.
main :: IO ()
main = do
    let !sbools = toSList bools
    defaultMain [
        bgroup "mapBool"
                [ bench "hs_apply"          $ nfIO (applyFunction eval_ sbools)
                , bench "cmm_hslike"        $ nfIO (applyFunctionHsLike eval_ sbools)
                , bench "cmm_direct"        $ nfIO (applyFunctionDirect eval_ sbools)
                -- , bench "5"  $ nf id fastMapBools
                ]
        ]