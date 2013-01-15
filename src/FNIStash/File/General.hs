-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.File.General
-- Copyright   :  2013 Daniel Austin
-- License     :  AllRightsReserved
--
-- Maintainer  :  dan@fluffynukeit.com
-- Stability   :  Development
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module FNIStash.File.General 
    (getTorchText,
     getTorchTextL,
     wordToFloat,
     wordToDouble,
     streamToHex,
     intToHex)
where

import qualified Data.Binary.Strict.Get as S
import qualified Data.Binary.Get as L
import qualified Data.Text as T
import Control.Applicative
import Data.Text.Encoding (decodeUtf16LE)
import Numeric
import qualified Data.ByteString as BS

import Data.Word (Word32, Word64)
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)


getTorchText :: S.Get T.Text
getTorchText = fromIntegral . (*2) <$> S.getWord16le >>= S.getByteString >>= \x -> return (decodeUtf16LE x)

getTorchTextL :: L.Get T.Text
getTorchTextL = fromIntegral . (*2) <$> L.getWord16le >>= L.getByteString >>= \x -> return (decodeUtf16LE x)

streamToHex :: BS.ByteString -> T.Text
streamToHex = T.pack . ("0x" ++) . concatMap ((" "++) . showHexWord) . BS.unpack

showHexWord word = case length $ showHex word "" of
    1 -> "0" ++ showHex word ""
    2 -> showHex word ""

intToHex i = T.pack ("0x" ++ padding ++ (showHex i ""))
        where padding = replicate (8 - (length $ showHex i "")) '0'

-- Here I copied the Word -> Float/Double solution found at the following link
-- http://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-floa

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

-- END copied stack overflow solution for Word -> Float/Double
