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
( getTorchText
, getTorchTextL
, getTorchString
, getTorchString1Byte
, maybeAction
, wordToFloat
, wordToDouble
, showHex
, streamToHex
, intToHex
, showListString
, runGetWithFail
, runGetSuppress
, getFloat
, fromStrict
, toStrict
, copyLazy
, copyStrict
)
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

showListString f = foldl (\a b -> a <> f b) (""::String)

getTorchText :: SG.Get T.Text
getTorchText = fromIntegral . (*2) <$> SG.getWord16le >>= SG.getByteString >>= return . decodeUtf16LE 

getTorchTextL :: LG.Get T.Text
getTorchTextL = fromIntegral . (*2) <$> LG.getWord16le >>= LG.getByteString >>= return . decodeUtf16LE 

getTorchString :: SG.Get String
getTorchString = T.unpack <$> getTorchText

getTorchText1Byte = fromIntegral . (*2) <$> SG.getWord8 >>= SG.getByteString >>= return . decodeUtf16LE
getTorchString1Byte = T.unpack <$> getTorchText1Byte

getFloat :: SG.Get Float
getFloat = (SG.getWord32le >>= (return . wordToFloat))

streamToHex :: SBS.ByteString -> T.Text
streamToHex = T.pack . ("0x" ++) . concatMap ((" "++) . wordToHex) . SBS.unpack

maybeAction bool action = if bool then Just <$> action else return Nothing

wordToHex word = case length $ showHex word "" of
    1 -> "0" ++ showHex word ""
    2 -> showHex word ""

intToHex i = ("0x" ++ padding ++ (showHex i ""))
        where padding = replicate (8 - (length $ showHex i "")) '0'


fromStrict bs = LBS.fromChunks [bs]
toStrict = (mconcat . LBS.toChunks)

copyLazy = LBS.copy
copyStrict = SBS.copy

-- Utilities for handling file Get errors

runGetWithFail :: String -> SG.Get a -> SBS.ByteString -> Either String a
runGetWithFail msg action dataBS =
    let result = SG.runGet action dataBS
    in case result of
        (Left errorStr, _) -> Left  (msg <> " <- " <> errorStr)
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
