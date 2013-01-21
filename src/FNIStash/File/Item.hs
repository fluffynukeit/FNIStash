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
    model <- getWord64le
    name <- getTorchText
    prefix <- getTorchText
    suffix <- getTorchText
    serial <- getLazyByteString 24
    bytes1 <- getLazyByteString 29
    nEnchants <- fromIntegral <$> getWord32le
    location <- fromIntegral <$> getWord16le
    bytes2 <- getLazyByteString 9
    bytes3 <- getLazyByteString 8
    bytes4 <- sequence $ replicate 4 $ getLazyByteString 20
    level <- fromIntegral <$> getWord32le
    bytes5 <- getLazyByteString 4
    nSockets <- fromIntegral <$> getWord32le
    nUsedSockets <- fromIntegral <$> getWord32le
    bytes6 <- getLazyByteString 4
    maxDmg <- fromIntegral <$> getWord32le
    armor <- fromIntegral <$> getWord32le
    bytes7 <- getLazyByteString 4
    bytes8 <- getLazyByteString 12
    nElements <- fromIntegral <$> getWord16le
    elements <- sequence $ replicate (fromIntegral nElements) $ getLazyByteString 12
    modLists <- getModLists
    return $ Item model name prefix suffix serial bytes1 nEnchants location bytes2 bytes3
                bytes4 level bytes5 nSockets nUsedSockets bytes6 maxDmg armor bytes7 bytes8
                nElements elements modLists


getMod :: Get Mod
getMod = do
    modType <- getWord32le
    modName <- getTorchText
    restData <- lkModReader modType
    return $ Mod modType modName restData

getModList :: Word32 -> Get [Mod]
getModList modCount = replicateM (fromIntegral modCount) getMod

getModLists = do
    modCount <- getWord32le
    if (modCount == 0) then -- there are no more lists to read in
        replicateM 3 getWord32le >> return []-- consume the remaining 12 0x00 bytes and finish
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
m8441 = m8041
m8141 = getLazyByteString 45
m8050 = m8041
m8058 = getLazyByteString 39
mA141 = getLazyByteString 53
mA041 = getLazyByteString 51
m8048 = getLazyByteString 39
m8042 = getLazyByteString 33
m9041 = getLazyByteString 59
m8051 = getLazyByteString 67
mDefault = getLazyByteString 45

modTypeSizeMap :: M.Map Word32 (Get BS.ByteString)
modTypeSizeMap = M.fromList [(0x804A, m804A), (0x8041, m8041), (0x8441, m8441), (0x8149, m8149),
                             (0x8050, m8050), (0x8141, m8141), (0x8058, m8058), (0xA141, mA141),
                             (0xA041, mA041), (0x8048, m8048), (0x8042, m8042), (0x9041, m9041),
                             (0x8051, m8051)]


lkModReader modType = M.findWithDefault mDefault modType modTypeSizeMap
