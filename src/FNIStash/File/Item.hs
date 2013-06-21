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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FNIStash.File.Item
    ( getItemBytes
    , putPartition
    , ItemBytes(..)
    , LocationBytes(..)
    , EffectBytes(..)
    , Partition(..)
    , AddedDamageBytes(..)
    , TriggerableBytes(..)
    ) where

import FNIStash.File.General

import qualified Data.ByteString as BS
import Data.Binary.Strict.Get
import Data.Binary.Put
import Control.Applicative
import Control.Monad.Loops
import Control.Monad
import Data.Word
import Data.Maybe


-- Utility function for getting a list of something with prepended length

getListOf (getter) = getWord32le >>= return . fromIntegral >>= flip replicateM getter


-- Top level for parsing ITEM

data ItemBytes = ItemBytes
    { iBytesGUID :: Word64
    , iBytesName :: String
    , iBytesPrefix :: String
    , iBytesSuffix :: String
    , iBytesRandomID :: BS.ByteString
    , iBytesNumEnchants :: Word32
    , iBytesLocation :: LocationBytes
    , iBytesIdentified :: Word8
    , iBytesLevel :: Word32
    , iBytesQuantity :: Word32
    , iBytesNumSockets :: Word32
    , iBytesGems :: [ItemBytes]
    , iBytesDamage :: Word32
    , iBytesArmor :: Word32
    , iBytesAddedDamages :: [AddedDamageBytes]
    , iBytesEffects :: [EffectBytes]
    , iBytesEffects2 :: [EffectBytes]
    , iBytesTriggerables :: [TriggerableBytes]
    , iBytesStats :: [StatBytes]
    , iBytesPartition :: Partition    -- Item data before and after location.
} deriving (Eq, Ord)

data Partition = Partition
    { pBeforeLocation :: BS.ByteString
    , pAfterLocation :: BS.ByteString
    } deriving (Eq, Ord)

getItemBytes :: BS.ByteString -> Get ItemBytes
getItemBytes itemBinaryData = do
    lead <- getWord8
    guid <- getWord64le
    name <- getTorchString
    prefix <- getTorchString
    suffix <- getTorchString
    randomID <- getByteString 24
    bytes0 <- getWord32le   -- added for new stash format
    bytes1 <- getByteString 29  -- not sure what these do... almost all FF
    nEnchants <- fromIntegral <$> getWord32le
    nBytesBeforeLocation <- bytesRead
    location <- getLocationBytes
    bytes2 <- getByteString 6 -- always 00 01 01 01 01 00 01?
    identified <- getWord8
    bytes3 <- getByteString 8 -- can be different for equal items..more than just 8
    bytes4 <- replicateM 4 $ getByteString 20
    level <- getWord32le
    quantity <- getWord32le
    nSockets <- getWord32le
    gems <- getListOf $ getItemBytes itemBinaryData
    bytes6 <- getByteString 4 -- always 00 00 00 00?
    maxDmg <- getWord32le
    armor <- getWord32le
    bytes7 <- getByteString 4 -- different for equal items?
    bytes8 <- getByteString 12 -- 12x FF
    nDmgTypes <- getWord16le
    addedDamages <- replicateM (fromIntegral nDmgTypes) getAddedDamageBytes
    effectList <- getEffectLists >>= return . concat
    effectList2 <- getEffectLists >>= return . concat
    
    trigList <- getListOf getTriggerableBytes
    statList <- getListOf getStatBytes

    return $ ItemBytes guid name prefix suffix randomID nEnchants location identified level
                       quantity nSockets gems maxDmg armor addedDamages effectList effectList2
                       trigList statList
                       $ Partition (BS.take nBytesBeforeLocation itemBinaryData)
                                   (BS.drop (nBytesBeforeLocation+4) itemBinaryData)

-- Parsing LOCATION

data LocationBytes = LocationBytes
    { lBytesSlotIndex :: Word16
    , lBytesContainer :: Word16
    } deriving (Eq, Ord)

getLocationBytes = LocationBytes <$> getWord16le <*> getWord16le
putLocationBytes (LocationBytes {..}) = putWord16le lBytesSlotIndex >> putWord16le lBytesContainer

-- Parsing ADDED DAMAGES, such as from enchants or sockets

data AddedDamageBytes = AddedDamageBytes
    { dBytesFromSocket :: Word32
    , dBytesFromEnchant :: Word32
    , dBytesDamageType :: Word32
    } deriving (Eq, Ord)

getAddedDamageBytes = AddedDamageBytes <$> getWord32le <*> getWord32le <*> getWord32le



-- Parsing the EFFECTS that add special behaviors

data EffectBytes = EffectBytes
    { eBytesType :: Word16
    , eBytesName :: String
    , eBytesFile :: Maybe String
    , eBytesGUID :: Maybe Word64
    , eBytesNumValues :: Word8
    , eBytesValueList :: [Word32]
    , eBytesIndex :: Word32
    , eBytesDamageType :: Word32
    , eBytesDescriptionType :: Word32
    , eBytesItemLevel :: Word32
    , eBytesDuration :: Word32
    , eBytesValue :: Word32
    , eBytesLink :: Word32 -- tells us whether the list continues
    , eBytesExtraString :: Maybe String
    } deriving (Eq, Ord)

getEffect = do
    mType <- getWord16le
    hasExtraStringBytes <- getWord16le -- uses these bytes to do a lookup to see if filepath is present
    mName <- getTorchString
    file <- maybeAction (hasFilePath mType) getTorchString
    guid <- maybeAction (hasGUID mType) getWord64le
    numVals <- getWord8
    mValueList <- replicateM (fromIntegral numVals) getWord32le
    mUnknown1 <- getTorchText -- This is always 00 00??
    mEffectIndex <- getWord32le
    mDmgType <- getWord32le
    mDescType <- getWord32le
    mItemLevel <- getWord32le
    mDuration <- getWord32le
    mUnknown3 <- getWord32le
    mValue <- getWord32le
    listLinkValue <- getWord32le
    extraString <- maybeAction (hasExtraString hasExtraStringBytes) getTorchString1Byte
    return $ EffectBytes mType mName file guid numVals mValueList mEffectIndex mDmgType mDescType
                         mItemLevel mDuration mValue listLinkValue extraString

getEffectList = do
    effList <- getListOf getEffect
    if length effList == 0 then
        return ([], False)
        else do
            let lastEffInList = last effList
                eatExtraByte = if 0x804a == (eBytesType lastEffInList)
                               then getWord32le >> return () -- consume the extra byte
                               else return ()
                anotherListNext = eBytesLink lastEffInList == 0x03
            eatExtraByte
            return (effList, anotherListNext)

getEffectLists :: Get [[EffectBytes]]
getEffectLists = do
    (thisList, hasNext) <- getEffectList
    if not hasNext then
        return [thisList]
        else do
            remainingLists <- getEffectLists 
            return $ (thisList:remainingLists)

hasFilePath 0x8141 = True
hasFilePath 0x8140 = True
hasFilePath 0xa141 = True
hasFilePath 0x9041 = True
hasFilePath 0x8541 = True
hasFilePath 0x8149 = True
hasFilePath 0x0140 = True
hasFilePath 0x2141 = True

hasFilePath 0xa041 = False
hasFilePath 0x8041 = False
hasFilePath 0x8050 = False
hasFilePath 0x0041 = False
hasFilePath 0x8440 = False -- this is on the +5% poison dmg enchant
hasFilePath _ = False

hasGUID 0xA141 = True
hasGUID 0xA041 = True
hasGUID 0x2141 = True
hasGUID _      = False

hasExtraString 0x02 = True
hasExtraString _    = False


-- Parsing the TRIGGERABLES (like counters on kill).  This is usually empty.

newtype TriggerableBytes = TriggerableBytes String deriving (Eq, Ord)

getTriggerableBytes = TriggerableBytes <$> getTorchString


-- Parsing the STATS related to item, like special kill count. This is usually empty.

data StatBytes = StatBytes
    { statGUID :: Word64
    , statBytes :: BS.ByteString -- Keep raw data because we only know data type
                                 -- after we do a lookup by GUID
    } deriving (Eq, Ord)

getStatBytes = StatBytes <$> getWord64le <*> getByteString 4

-- TODO look out! I think only item data in the shared stash file is prefixed by its length
putPartition :: LocationBytes -> Partition -> Put
putPartition locBytes (Partition {..}) = do
    let dataLength = BS.length pBeforeLocation + BS.length pAfterLocation + 4
    putWord32le $ fromIntegral dataLength
    putByteString pBeforeLocation
    putLocationBytes locBytes
    putByteString pAfterLocation

