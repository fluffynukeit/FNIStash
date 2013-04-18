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

module FNIStash.File.Item (
    getItem,
    putItem,
    showItem,
    moveTo,
    showMod,
    Item(..),
    Location(..)
) where

import FNIStash.File.General
import FNIStash.Logic.Translate
import FNIStash.Logic.Env
import FNIStash.File.Variables
import FNIStash.File.DAT
import FNIStash.File.Location

import qualified Data.ByteString as BS
import Data.Binary.Strict.Get
import Data.Binary.Put
import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Data.Word
import Data.Int
import Data.Monoid hiding (All)
import Data.Maybe

data Item = Item
    { itemGUID :: Int64
    , itemRandomID :: BS.ByteString
    , itemName :: String
    , itemNumEnchants :: Int
    , itemLevel :: Int
    , itemNumSockets :: Int
    , itemGems :: [Item]
    , itemPoints :: Int
    , itemDamageTypes :: [DamageType]
    , itemMods :: [Mod]
    , itemLocation :: Location
    , itemDataPieces :: (BS.ByteString, BS.ByteString)    -- Item data before and after location.
    , itemIcon :: String
} deriving (Eq, Ord, Show)


data Mod = Mod {
    modType :: Word32,
    modName :: String,
    modValueList :: [Float],
    modEffectIndex :: Word32,
    modDamageType :: DamageType,
    modItemLevel :: Word32,
    modDuration :: Float,
    modValue :: Float,
    modClass :: ModClass,
    modText :: String,
    modPrecision :: Int
} deriving (Eq, Ord, Show)


data DamageType = Physical | Fire | Electric | Ice | Poison | All | Unknown
    deriving (Ord, Show, Eq)

data ModClass = Normal | Innate | Augment
    deriving (Ord, Eq, Show)

moveTo loc (Item {..}) =
    Item itemGUID itemRandomID itemName itemNumEnchants itemLevel itemNumSockets itemGems itemPoints
         itemDamageTypes itemMods loc itemDataPieces itemIcon


getItem :: Env -> BS.ByteString -> Get Item
getItem env itemBinaryData = do
    lead <- getWord8
    guid <- fromIntegral <$> getWord64le
    name <- getTorchString
    prefix <- getTorchString
    suffix <- getTorchString
    randomID <- getByteString 24
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
    -- every item ends in 16 00 bytes?  Only read 12 more bytes because 4 were
    -- consumed by identifying the end of the mod lists
    replicateM 3 getWord32le
    let iconName = getIconName env guid
    return $ Item guid randomID (unwords [name, prefix, suffix]) (fromIntegral nEnchants) level
                      (fromIntegral nSockets) gems (fromIntegral (if maxDmg == 0xFFFFFFFF then armor else maxDmg))
                      elements modLists location
                      (BS.take nBytesBeforeLocation itemBinaryData,
                       BS.drop (nBytesBeforeLocation+4) itemBinaryData)
                       iconName

getDamageType = do
    getByteString 8     -- 8 leading bytes are always 0? - No, look at raleigh claire
    dmgType <- getWord32le
    return $ damageTypeLookup dmgType

getMod env = do
    mType <- getWord32le
    mName <- getTorchString
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


-- TODO look out! I think only item data in the shared stash file is prefixed by its length
putItem :: Env -> Item -> Put
putItem env item = do
    let data1 = fst $ itemDataPieces item
        data2 = snd $ itemDataPieces item
        dataLength = BS.length data1 + BS.length data2 + 4
    putWord32le $ fromIntegral dataLength
    putByteString data1
    putLocation env $ itemLocation item
    putByteString data2


getIconName env guid =
    let lkupID = lkupItemGUID env -- looks up an item by Int64 GUID
        lkupPath = lkupItemPath env
        findIcon guid =
            let item = lkupID guid
                maybeIcon = item >>= lkupVar vICON >>= stringVar
                baseItemID = fromJust $ item >>=
                                        lkupVar vBASEFILE >>=
                                        textVar >>= lkupPath >>=
                                        lkupVar vUNIT_GUID >>= stringVar >>= return . read
            in  case maybeIcon of
                Nothing -> findIcon baseItemID
                Just icon -> icon
    in findIcon guid

instance Translate Mod where
    translateMarkup mod markup =
        let prec = stringPrecision $ modPrecision mod
            r = roundAt $ modPrecision mod
            dispVal = prec . show . r
        in (case markup of
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


showItem i = unlines
    ["GUID: " <> (show $ (fromIntegral $ itemGUID i::Int64)),
     "Full name: " <> itemName i,
     "Icon: " <> itemIcon i,
     "Location: " <> show (itemLocation i),
     "Num Enchants: " <> show (itemNumEnchants i),
     "Item level: " <> show (itemLevel i),
     "Used Sockets: " <> (show $ (length . itemGems) i) <> "/" <> (show $ itemNumSockets i),
     "Dmg/Armor: " <> (show $ itemPoints i),
     "Num elements: " <> (show $ (length . itemDamageTypes) i),
     "Mods: " ,"", showListString showMod $ itemMods i,
     "Gems: " <> (showListString showItem $ itemGems i),
     "", ""]

showMod = modText

modDescription env mod =
    let effectNode = (lkupEffect env) (modEffectIndex mod)
        maybeSentence = (effectNode >>= lkupVar vGOODDES >>= stringVar)
    in case maybeSentence of
        Just sentence -> unlines [translateSentence mod sentence]
        Nothing -> modDataDump mod

modDataDump i =
    let k f = show $ f i
    in   unlines
        ["","Type: " <> (intToHex . fromIntegral $ modType i),
         "Name: " <> (modName i),
         "Values: " <> (showListString (\x -> (show x) <> ", ") $ modValueList i),
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
