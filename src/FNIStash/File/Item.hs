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

module FNIStash.File.Item (
    getItem,
    textItem,
    Item -- re-export
) where

import FNIStash.File.General
import FNIStash.Logic.Translate
import FNIStash.File.Variables
import FNIStash.File.DAT

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Data.Binary.Get
import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Data.Word
import Data.Int
import Data.Monoid hiding (All)



data Item = Item {
    leadByte :: Word8,
    guid :: Word64,
    name :: T.Text,
    prefix :: T.Text,
    suffix :: T.Text,
    serial :: BS.ByteString,
    bytes1 :: BS.ByteString, -- 00 FFx24 00 4x
    nEnchants :: Word32,
    location :: Word16,
    bytes2 :: BS.ByteString, -- 18 00 00 01 01 01 01 00 01
    bytes3 :: BS.ByteString,
    bytes4 :: [BS.ByteString],
    level :: Word32,
    bytes5 :: BS.ByteString, -- 01 00 00 00 always?
    nSockets :: Word32,
    nSocketsUsed :: Word32,
    bytes6 :: BS.ByteString,
    maxDmg :: Word32,
    armor :: Word32,
    bytes7 :: BS.ByteString,
    bytes8 :: BS.ByteString, -- FFx12
    nElements :: Word16,
    elements :: [BS.ByteString],
    mods :: [[Mod]],
    gems :: [Item]
    }

data Mod = Mod {
    modType :: Word32,
    modName :: T.Text,
    modValueList :: [Float],
    modUnknown1 :: T.Text,
    modEffectIndex :: Word32,
    modDamageType :: DamageType,
    modUnknown2 :: Word32,
    modItemLevel :: Word32,
    modDuration :: Float,
    modUnknown3 :: Word32,
    modValue :: Float,
    modUnknown4 :: Word32
}

data DamageType = Physical | Fire | Electric | Ice | Poison | All | Unknown
    deriving (Show, Eq)

instance Translate Mod where
    translateMarkup mod markup =
        T.pack $ (case markup of
            "VALUE" -> show . modValue
            "DURATION" -> show . modDuration
            "DMGTYPE" -> show . modDamageType
            "VALUE1" -> show . (flip (!!) 1) . modValueList
            "VALUE2" -> show . (flip (!!) 2) . modValueList
            "VALUE3" -> show . (flip (!!) 3) . modValueList
            "VALUE4" -> show . (flip (!!) 4) . modValueList
            "VALUE3AND4" -> show . (flip (!!) 3) . modValueList
            _ -> \mod -> "???"
            ) mod


textItem effSearch i = T.unlines
    ["GUID: " <> (T.pack . show $ (fromIntegral $ guid i::Int64)),
     "Full name: " <> (T.unwords [prefix i, name i, suffix i]),
     "Num Enchants: " <> T.pack (show $ nEnchants i),
     "Item level: " <> T.pack (show $ level i),
     "Used Sockets: " <> T.pack (show $ nSocketsUsed i) <> "/" <> T.pack (show $ nSockets i),
     "Dmg/Armor: " <> T.pack (show $ maxDmg i) <> "/" <> T.pack (show $ armor i),
     "Num elements: " <> T.pack (show $ nElements i),
     "Mods: " ,"", textList (textList (textMod effSearch)) (mods i),
     "Gems: " <> textList (textItem effSearch) (gems i),
     "", ""]

textMod effSearch i =
    let effectNode = effSearch (modEffectIndex i)
        maybeSentence = (effectNode >>= findVar vGOODDES >>= translateVar)
    in case maybeSentence of
        Just sentence -> T.unlines [translateSentence i sentence]
        Nothing -> textModOld i

textModOld i =
    let k f = T.pack . show $ f i
    in T.unlines
        ["","Type: " <> (intToHex . fromIntegral $ modType i),
         "Name: " <> (modName i),
         "Values: " <> (textList (\x -> (T.pack $ show x) <> ", ") $ modValueList i),
         "Unknown1: " <> k modUnknown1,
         "EffectIndex: " <> k modEffectIndex,
         "DmgType: " <> k modDamageType,
         "Unknown2: " <> k modUnknown2 ,
         "ItemLevel: " <> k modItemLevel,
         "Duration: " <> k modDuration,
         "Unknown3: " <> k modUnknown3,
         "Value: " <> k modValue,
         "Unknown4: " <> k modUnknown4,
         ""]


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
    numVals <- iterateUntil (/= 0) getWord8
    mValueList <- replicateM (fromIntegral numVals) getFloat
    mUnknown1 <- getTorchText
    mEffectIndex <- getWord32le
    mDmgType <- getWord32le >>= return . damageTypeLookup
    mUnknown2 <- getWord32le
    mItemLevel <- getWord32le
    mDuration <- getFloat
    mUnknown3 <- getWord32le
    mValue <- getFloat
    mUnknown4 <- getWord32le
    return $ Mod mType mName mValueList mUnknown1 mEffectIndex mDmgType
                 mUnknown2 mItemLevel mDuration mUnknown3 mValue mUnknown4

damageTypeLookup 0x00 = Physical
damageTypeLookup 0x02 = Fire
damageTypeLookup 0x03 = Ice
damageTypeLookup 0x04 = Electric
damageTypeLookup 0x05 = Poison
damageTypeLookup 0x06 = All
damageTypeLookup _ = Unknown

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


