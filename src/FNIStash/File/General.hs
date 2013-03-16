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
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module FNIStash.File.General 
    (getTorchText,
     getTorchTextL,
     wordToFloat,
     wordToDouble,
     showHex,
     streamToHex,
     intToHex,
     textList,
     runGetWithFail,
     runGetSuppress,
     getFloat,
     fromStrict,
     toStrict)
where

-- General helper functions for file operations

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf16LE)
import qualified Data.Binary.Strict.Get as SG
import qualified Data.Binary.Get as LG
import Numeric
import Control.Applicative
import Data.Word
import Data.Monoid

import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)

import Filesystem.Path.CurrentOS

textList f = foldl (\a b -> a <> f b) T.empty

getTorchText :: SG.Get T.Text
getTorchText = fromIntegral . (*2) <$> SG.getWord16le >>= SG.getByteString >>= \x -> return (decodeUtf16LE x)

getTorchTextL :: LG.Get T.Text
getTorchTextL = fromIntegral . (*2) <$> LG.getWord16le >>= LG.getByteString >>= \x -> return (decodeUtf16LE x)

getFloat :: SG.Get Float
getFloat = (SG.getWord32le >>= (return . wordToFloat))

streamToHex :: SBS.ByteString -> T.Text
streamToHex = T.pack . ("0x" ++) . concatMap ((" "++) . wordToHex) . SBS.unpack

wordToHex word = case length $ showHex word "" of
    1 -> "0" ++ showHex word ""
    2 -> showHex word ""

intToHex i = T.pack ("0x" ++ padding ++ (showHex i ""))
        where padding = replicate (8 - (length $ showHex i "")) '0'


fromStrict bs = LBS.fromChunks [bs]
toStrict = (mconcat . LBS.toChunks)

-- Utilities for handling file Get errors

runGetWithFail :: T.Text -> SG.Get a -> SBS.ByteString -> Either T.Text a
runGetWithFail msg action dataBS =
    let result = SG.runGet action dataBS
    in case result of
        (Left errorStr, _) -> Left  (msg <> " <- " <> (T.pack errorStr))
        (Right record, _)  -> Right record

runGetSuppress :: SG.Get a -> SBS.ByteString -> a
runGetSuppress action dataBS =
    case SG.runGet action dataBS of
        (Right a, rem) -> a

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
