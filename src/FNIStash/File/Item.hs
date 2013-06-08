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
    moveTo,
    Item(..),
    itemAsBS,
    itemLeadData,
    itemTrailData,
    effectText,
    effectValue,
    module FNIStash.File.Location
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
    , itemEffects :: [Effect]
    , itemTriggerables :: [Triggerable]
    , itemStats :: [Stat]
    , itemLocation :: Location
    , itemDataPieces :: (BS.ByteString, BS.ByteString)    -- Item data before and after location.
    , itemIcon :: String
} deriving (Eq, Ord)


data Effect = Effect {
    effectType :: Word16,
    effectSkillName :: Maybe String,
    effectValueList :: [Float],
    effectIndex :: Word32,
    effectDamageType :: DamageType,
    effectDescriptionType :: DescriptionType,
    effectItemLevel :: Word32,
    effectDuration :: Float,
    effectValue :: Float,
    effectText :: String,
    effectPrecision :: Int,
    effectSpecialGUID :: Maybe Int64,
    effectExtraString :: Maybe String
} deriving (Eq, Ord)

instance Show Effect where
    show = effectText


data DescriptionType = GOODDES | BADDES | GOODDESOT | BADDESOT
    deriving (Eq, Ord, Show)

data DamageType = Physical | Fire | Electric | Ice | Poison | All | Unknown
    deriving (Ord, Show, Eq)

data ModClass = Normal | Innate | Augment
    deriving (Ord, Eq, Show)

moveTo loc (Item {..}) =
    Item itemGUID itemRandomID itemName itemNumEnchants itemLevel itemNumSockets itemGems itemPoints
         itemDamageTypes itemEffects itemTriggerables itemStats loc itemDataPieces itemIcon


itemLeadData = fst . itemDataPieces
itemTrailData = snd . itemDataPieces

getItem :: Env -> BS.ByteString -> Get Item
getItem env itemBinaryData = do
    lead <- getWord8
    guid <- fromIntegral <$> getWord64le
    name <- getTorchString
    prefix <- getTorchString
    suffix <- getTorchString
    randomID <- getByteString 24
    bytes0 <- getWord32le   -- added for new stash format
    bytes1 <- getByteString 29  -- not sure what these do... almost all FF
    nEnchants <- getWord32le
    nBytesBeforeLocation <- bytesRead
    location <- getLocation env
    bytes2 <- getByteString 7 -- always 00 01 01 01 01 00 01?
    bytes3 <- getByteString 8 -- can be different for equal items..more than just 8
    bytes4 <- replicateM 4 $ getByteString 20
    level <- fromIntegral <$> getWord32le
    bytes5 <- getByteString 4 -- always 01 00 00 00?
    nSockets <- getWord32le
    nUsedSockets <-  getWord32le
    gems <- replicateM (fromIntegral nUsedSockets) $ getItem env itemBinaryData
    bytes6 <- getByteString 4 -- always 00 00 00 00?
    maxDmg <- getWord32le
    armor <- getWord32le
    bytes7 <- getByteString 4 -- different for equal items?
    bytes8 <- getByteString 12 -- 12x FF
    nElements <- getWord16le
    elements <- replicateM (fromIntegral nElements) getDamageType
    effectLists <- getEffectLists env >>= return . concat
    effectList2 <- getEffectLists env >>= return . concat
    
    trigList <- getListOf getTriggerable
    statList <- getListOf getStat
    
    let iconName = getIconName env guid
    return $ Item guid randomID (unwords [name, prefix, suffix]) (fromIntegral nEnchants) level
                      (fromIntegral nSockets) gems (fromIntegral (if maxDmg == 0xFFFFFFFF then armor else maxDmg))
                      elements effectLists trigList statList
                      location
                      (BS.take nBytesBeforeLocation itemBinaryData,
                       BS.drop (nBytesBeforeLocation+4) itemBinaryData)
                       iconName

getDamageType = do
    getByteString 8     -- 8 leading bytes are always 0? - No, look at raleigh claire
    dmgType <- getWord32le
    return $ damageTypeLookup dmgType

getEffect (env@Env{..}) = do
    mType <- getWord16le
    hasExtraStringBytes <- getWord16le
    mName <- getTorchText
    twoBlankBytes <- lookAhead getWord16le
    when (twoBlankBytes == 0x00) (getWord16le >> return ())
    guid <- if hasGUID mType
            then getWord64le >>= (return . Just . fromIntegral)
            else return Nothing 
    numVals <- getWord8
    mValueList <- replicateM (fromIntegral numVals) getFloat
    mUnknown1 <- getTorchText
    mEffectIndex <- getWord32le
    mDmgType <- getWord32le >>= return . damageTypeLookup
    mDescType <- getWord32le >>= return . descTypeLookup
    mItemLevel <- getWord32le
    mDuration <- getFloat
    mUnknown3 <- getWord32le
    mValue <- getFloat
    listLinkValue <- getWord32le
    extraString <- if hasExtraString hasExtraStringBytes
                   then Just <$> getTorchString1Byte
                   else return Nothing
    let dispName = lkupSkill mName >>= lkupVar vDISPLAYNAME >>= stringVar
        effect = Effect mType dispName mValueList mEffectIndex mDmgType mDescType
                  mItemLevel mDuration mValue text prec guid extraString
        text = effectDescription env effect
        prec = effectPrecisionVal env effect
        
    return (effect, listLinkValue)

hasGUID 0xA141 = True
hasGUID 0xA041 = True
hasGUID _      = False

hasExtraString 0x02 = True
hasExtraString _    = False


getListOf (getter) = getWord32le >>= return . fromIntegral >>= flip replicateM getter

getEffectList env = do
    effectCount <- getWord32le
    effectLinkPairs <- replicateM (fromIntegral effectCount) (getEffect env)
    if length effectLinkPairs == 0 then
        return ([], False)
        else do
            let finalLinkVal = last $ map snd effectLinkPairs
                effectsOnly = map fst effectLinkPairs
                eatExtraByte = if 0x804a == (effectType $ last effectsOnly)
                               then getWord32le >> return ()
                               else return ()
                anotherListNext = finalLinkVal == 0x03
            eatExtraByte
            return (effectsOnly, anotherListNext)

getEffectLists :: Env -> Get [[Effect]]
getEffectLists env = do
    (thisList, hasNext) <- getEffectList env
    if not hasNext then
        return [thisList]
        else do
            remainingLists <- getEffectLists env 
            return $ (thisList:remainingLists)
--    modCount <- getWord32le
--    if (modCount == 0) then
--        return []
--        else do
--            thisListPair <- replicateM (fromIntegral modCount) (getEffect env)
--            remainingLists <- getEffectLists env
--            return $ ((map fst thisListPair):remainingLists)

getTriggerable = Triggerable <$> getTorchString

getStat = Stat <$> (getWord64le >>= return . fromIntegral) <*> getByteString 4

data Triggerable = Triggerable String deriving (Eq, Ord)
instance Show Triggerable where
    show (Triggerable a) = a

data Stat = Stat
    { statGUID :: Int64
    , statBytes :: BS.ByteString -- Keep raw data because we only know data type
                                 -- after we do a lookup by GUID
    } deriving (Eq, Ord)

instance Show Stat where
    show (Stat i b) = show i ++ ":" ++ show b  

damageTypeLookup 0x00 = Physical
damageTypeLookup 0x02 = Fire
damageTypeLookup 0x03 = Ice
damageTypeLookup 0x04 = Electric
damageTypeLookup 0x05 = Poison
damageTypeLookup 0x06 = All
damageTypeLookup _ = Unknown

descTypeLookup 0x00 = GOODDES
descTypeLookup 0x01 = GOODDES
descTypeLookup 0x02 = GOODDESOT
descTypeLookup 0x03 = BADDES  -- This is just a guess
descTypeLookup 0x04 = BADDESOT -- This is also a guess


-- TODO look out! I think only item data in the shared stash file is prefixed by its length
putItem :: Env -> Item -> Put
putItem env item = do
    let data1 = itemLeadData item
        data2 = itemTrailData item
        dataLength = BS.length data1 + BS.length data2 + 4
    putWord32le $ fromIntegral dataLength
    putByteString data1
    putLocation env $ itemLocation item
    putByteString data2


itemAsBS env item = runPut (putItem env item)

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

instance Translate Effect where
    translateMarkup (Effect{..}) markup =
        let prec = stringPrecision effectPrecision
            r = roundAt effectPrecision
            dispVal = prec . show . r
        in case markup of
            "VALUE"     -> "VALUE" -- leave VALUE unchanged so we can store in DB smarter
            "DURATION"  -> dispVal effectDuration
            "DMGTYPE"   -> show effectDamageType
            "VALUE1"    -> dispVal $ (flip (!!) 0) effectValueList
            "VALUE2"    -> dispVal $ (flip (!!) 1) effectValueList
            "VALUE3"    -> dispVal $ (flip (!!) 2) effectValueList
            "VALUE4"    -> dispVal $ (flip (!!) 3) effectValueList
            "VALUE3AND4" -> dispVal $ (flip (!!) 3) effectValueList
            "NAME"      -> maybe ("?Name?") id effectSkillName
            _           -> "???"
            

instance Show Item where
    show i = unlines
        ["GUID: " <> (show $ (fromIntegral $ itemGUID i::Int64)),
         "Full name: " <> itemName i,
         "Icon: " <> itemIcon i,
         "Location: " <> show (itemLocation i),
         "Num Enchants: " <> show (itemNumEnchants i),
         "Item level: " <> show (itemLevel i),
         "Used Sockets: " <> (show $ (length . itemGems) i) <> "/" <> (show $ itemNumSockets i),
         "Dmg/Armor: " <> (show $ itemPoints i),
         "Num elements: " <> (show $ (length . itemDamageTypes) i),
         "Effects: " ,"", showListString show $ itemEffects i,
         "Gems: " <> (showListString show $ itemGems i),
         "", ""]

effectDescription env (effect@Effect {..}) =
    let effectNode = lkupEffect env effectIndex
        descType = case effectDescriptionType of
            GOODDES     -> vGOODDES
            GOODDESOT   -> vGOODDESOT
            BADDES      -> vBADDES
            BADDESOT    -> vBADDESOT
        maybeSentence = (effectNode >>= lkupVar descType >>= stringVar)
    in case maybeSentence of
        Just sentence -> translateSentence effect sentence
        Nothing -> effectDataDump effect

effectDataDump (Effect {..}) =
    unlines
    ["","Type: " <> (intToHex . fromIntegral $ effectType),
     "Name: " <> (show effectSkillName),
     "Values: " <> (showListString (\x -> (show x) <> ", ") effectValueList),
     "EffectIndex: " <> show effectIndex,
     "DmgType: " <> show effectDamageType,
     "ItemLevel: " <> show effectItemLevel,
     "Duration: " <> show effectDuration,
     "Value: " <> show effectValue,
     ""]

effectPrecisionVal env effect =
    let effectNode = (lkupEffect env) (effectIndex effect)
        maybePrecision = (effectNode >>= lkupVar vDISPLAYPRECISION >>= intVar)
    in maybe 1 id maybePrecision

roundAt prec val = ((fromIntegral . ceiling) (val*10^prec)) / 10^prec

stringPrecision prec string =
    let (preDecimal, postDecimal) = break (== '.') string
    in if prec == 0
        then preDecimal
        else preDecimal ++ (take (prec+1) postDecimal)
