-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.File.Item
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

module FNIStash.File.Item (
    getItem,
    Item -- re-export
) where

import FNIStash.File.General
import FNIStash.Logic.Item

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import Data.Binary.Get
import Control.Applicative
import Control.Monad
import Data.Word
import Data.Monoid

getItem :: Get Item
getItem = do
    lead <- getWord8
    model <- getWord64le
    name <- getTorchText
    prefix <- getTorchText
    suffix <- getTorchText
    serial <- getLazyByteString 24
    bytes1 <- getLazyByteString 29
    nEnchants <- getWord32le
    location <- getWord16le
    bytes2 <- getLazyByteString 9
    bytes3 <- getLazyByteString 8
    bytes4 <- replicateM 4 $ getLazyByteString 20
    level <- fromIntegral <$> getWord32le
    bytes5 <- getLazyByteString 4 -- always 01 00 00 00?
    nSockets <- getWord32le
    nUsedSockets <-  getWord32le
    innerItems <- replicateM (fromIntegral nUsedSockets) getItem
    bytes6 <- getLazyByteString 4
    maxDmg <- getWord32le
    armor <- getWord32le
    bytes7 <- getLazyByteString 4
    bytes8 <- getLazyByteString 12
    nElements <- getWord16le
    elements <- replicateM (fromIntegral nElements) $ getLazyByteString 12
    modLists <- getModLists
    return $ Item lead model name prefix suffix serial bytes1 nEnchants location bytes2 bytes3
                bytes4 level bytes5 nSockets nUsedSockets bytes6 maxDmg armor bytes7 bytes8
                nElements elements modLists innerItems


getMod :: Get Mod
getMod = do
    mType <- getWord32le
    mName <- getTorchText
    mValueList <- getWord8 >>= \x -> replicateM (fromIntegral x) getFloat
    mUnknown1 <- getTorchText
    mEffectIndex <- getWord32le
    mDmgType <- getWord32le
    mUnknown2 <- getWord32le
    mItemLevel <- getWord32le
    mDuration <- getFloat
    mUnknown3 <- getWord32le
    mValue <- getFloat
    mUnknown4 <- getWord32le
    return $ Mod mType mName mValueList mUnknown1 mEffectIndex mDmgType
                 mUnknown2 mItemLevel mDuration mUnknown3 mValue mUnknown4

getModList :: Word32 -> Get [Mod]
getModList modCount = replicateM (fromIntegral modCount) getMod

getModLists = do
    modCount <- getWord32le
    if (modCount == 0) then -- there are no more lists to read in
        return []-- finish
        else do
            thisList <- getModList modCount
            remainingLists <- getModLists
            return (thisList:remainingLists)

m804A = getLazyByteString 39
m8149 = getLazyByteString 45
m8041 = do
    count <- getWord8
    mainBS <- getLazyByteString (30 + 4* fromIntegral count + 4)
    return (BS.singleton count <> mainBS)
m108041 = getLazyByteString 47
m8441 = m8041
m8141 = getLazyByteString 45
m8050 = m8041
m8058 = getLazyByteString 39 -- 35?
mA141 = getLazyByteString 53
mA041 = getLazyByteString 51
m8048 = getLazyByteString 39
m8042 = getLazyByteString 33
m9041 = getLazyByteString 59
m8051 = getLazyByteString 67
m282141 = getLazyByteString 57
m088141 = getLazyByteString 45
m088140 = m088141
m300041 = getLazyByteString 43
m300141 = getLazyByteString 45
mDefault = getLazyByteString 45

modTypeSizeMap :: M.Map Word32 (Get BS.ByteString)
modTypeSizeMap = M.fromList [(0x804A, m804A), (0x8041, m8041), (0x8441, m8441), (0x8149, m8149),
                             (0x8050, m8050), (0x8141, m8141), (0x8058, m8058), (0xA141, mA141),
                             (0xA041, mA041), (0x8048, m8048), (0x8042, m8042), (0x9041, m9041),
                             (0x8051, m8051), (0x108041, m108041), (0x282141, m282141), (0x088141, m088141),
                             (0x300041, m300041), (0x300141, m300141), (0x088140, m088140)]


lkModReader modType = M.findWithDefault mDefault modType modTypeSizeMap
