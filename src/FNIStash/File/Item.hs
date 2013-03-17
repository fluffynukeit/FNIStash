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
import FNIStash.Logic.Env
import FNIStash.File.Variables
import FNIStash.File.DAT
import FNIStash.File.Location

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Binary.Strict.Get
import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Data.Word
import Data.Int
import Data.Monoid hiding (All)



data Item = Item {
    itemGUID :: Word64,
    itemFullName :: T.Text,
    itemNumEnchants :: Int,
    itemLevel :: Int,
    itemNumSockets :: Int,
    itemGems :: [Item],
    itemPoints :: Int,
    itemDamageTypes :: [DamageType],
    itemMods :: [Mod],
    itemLocation :: Location,
    itemDataPieces :: (BS.ByteString, BS.ByteString)    -- Item data before and after location.
}


data Mod = Mod {
    modType :: Word32,
    modName :: T.Text,
    modValueList :: [Float],
--    modUnknown1 :: T.Text,
    modEffectIndex :: Word32,
    modDamageType :: DamageType,
--    modUnknown2 :: Word32,
    modItemLevel :: Word32,
    modDuration :: Float,
--    modUnknown3 :: Word32,
    modValue :: Float,
--    modUnknown4 :: Word32
    modClass :: ModClass,
    modText :: T.Text,
    modPrecision :: Int
}


data DamageType = Physical | Fire | Electric | Ice | Poison | All | Unknown
    deriving (Show, Eq)

data ModClass = Normal | Innate | Augment

getItem :: Env -> BS.ByteString -> Get Item
getItem env itemBinaryData = do
    lead <- getWord8
    model <- getWord64le
    name <- getTorchText
    prefix <- getTorchText
    suffix <- getTorchText
    serial <- getByteString 24
    bytes1 <- getByteString 29  -- not sure what these do
    nEnchants <- getWord32le
    nBytesBeforeLocation <- bytesRead
    location <- getLocation env
    bytes2 <- getByteString 7
    bytes3 <- getByteString 8
    bytes4 <- replicateM 4 $ getByteString 20
    level <- fromIntegral <$> getWord32le
    bytes5 <- getByteString 4 -- always 01 00 00 00?
    nSockets <- getWord32le
    nUsedSockets <-  getWord32le
    gems <- replicateM (fromIntegral nUsedSockets) $ getItem env itemBinaryData
    bytes6 <- getByteString 4
    maxDmg <- getWord32le
    armor <- getWord32le
    bytes7 <- getByteString 4
    bytes8 <- getByteString 12
    nElements <- getWord16le
    elements <- replicateM (fromIntegral nElements) getDamageType
    modLists <- getModLists env >>= return . concat
    return $ Item model (T.unwords [name, prefix, suffix]) (fromIntegral nEnchants) level
                      (fromIntegral nSockets) gems (fromIntegral (if maxDmg == 0xFFFFFFFF then armor else maxDmg))
                      elements modLists location
                      (BS.take nBytesBeforeLocation itemBinaryData,
                       BS.drop (nBytesBeforeLocation+2) itemBinaryData)

getDamageType = do
    getByteString 8     -- 8 leading bytes are always 0?
    dmgType <- getWord32le
    return $ damageTypeLookup dmgType

getMod env = do
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
    let mod = Mod mType mName mValueList mEffectIndex mDmgType
                  mItemLevel mDuration mValue Normal text prec
        text = modDescription env mod
        prec = modPrecisionVal env mod
    return mod
                 
getModList env modCount = replicateM (fromIntegral modCount) (getMod env)

getModLists :: Env -> Get [[Mod]]
getModLists env = do
    modCount <- getWord32le
    if (modCount == 0) then -- there are no more lists to read in
        return []-- finish
        else do
            thisList <- getModList env modCount
            remainingLists <- getModLists env
            return $ (thisList:remainingLists)


damageTypeLookup 0x00 = Physical
damageTypeLookup 0x02 = Fire
damageTypeLookup 0x03 = Ice
damageTypeLookup 0x04 = Electric
damageTypeLookup 0x05 = Poison
damageTypeLookup 0x06 = All
damageTypeLookup _ = Unknown

instance Translate Mod where
    translateMarkup mod markup =
        let prec = stringPrecision $ modPrecision mod
            r = roundAt $ modPrecision mod
            dispVal = prec . show . r
        in T.pack $ (case markup of
            "VALUE" -> dispVal . modValue
            "DURATION" -> dispVal . modDuration
            "DMGTYPE" -> show . modDamageType
            "VALUE1" -> dispVal . (flip (!!) 1) . modValueList
            "VALUE2" -> dispVal . (flip (!!) 2) . modValueList
            "VALUE3" -> dispVal . (flip (!!) 3) . modValueList
            "VALUE4" -> dispVal . (flip (!!) 4) . modValueList
            "VALUE3AND4" -> dispVal . (flip (!!) 3) . modValueList
            _ -> \mod -> "???"
            ) mod


textItem i = T.unlines
    ["GUID: " <> (T.pack . show $ (fromIntegral $ itemGUID i::Int64)),
     "Full name: " <> itemFullName i,
     "Location: " <> (T.pack $ show  (itemLocation i) ),
     "Num Enchants: " <> T.pack (show $ itemNumEnchants i),
     "Item level: " <> T.pack (show $ itemLevel i),
     "Used Sockets: " <> T.pack (show $ (length . itemGems) i) <> "/" <> T.pack (show $ itemNumSockets i),
     "Dmg/Armor: " <> T.pack (show $ itemPoints i),
     "Num elements: " <> T.pack (show $ (length . itemDamageTypes) i),
     "Mods: " ,"", textList textMod $ itemMods i,
     "Gems: " <> (textList textItem $ itemGems i),
     "", ""]

textMod = modText

modDescription env mod =
    let effectNode = (lkupEffect env) (modEffectIndex mod)
        maybeSentence = (effectNode >>= lkupVar vGOODDES >>= translateVar)
    in case maybeSentence of
        Just sentence -> T.unlines [translateSentence mod sentence]
        Nothing -> modDataDump mod

modDataDump i =
    let k f = T.pack . show $ f i
    in T.unlines
        ["","Type: " <> (intToHex . fromIntegral $ modType i),
         "Name: " <> (modName i),
         "Values: " <> (textList (\x -> (T.pack $ show x) <> ", ") $ modValueList i),
         "EffectIndex: " <> k modEffectIndex,
         "DmgType: " <> k modDamageType,
         "ItemLevel: " <> k modItemLevel,
         "Duration: " <> k modDuration,
         "Value: " <> k modValue,
         ""]

modPrecisionVal env mod =
    let effectNode = (lkupEffect env) (modEffectIndex mod)
        maybePrecision = (effectNode >>= lkupVar vDISPLAYPRECISION >>= intVar)
    in maybe 1 id maybePrecision

roundAt prec val = ((fromIntegral . ceiling) (val*10^prec)) / 10^prec

stringPrecision prec string =
    let (preDecimal, postDecimal) = break (== '.') string
    in if prec == 0
        then preDecimal
        else preDecimal ++ (take (prec+1) postDecimal)
