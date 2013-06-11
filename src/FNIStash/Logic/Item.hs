-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.Logic.Item
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

module FNIStash.Logic.Item
    ( getItem
    , putItem
    , Item(..)
    , ItemBase(..)
    , Location(..)
    , Partition(..)
    , Mod(..)
    ) where

-- This file is for decodeing raw bytes of items into useables types and useable information

import FNIStash.File.Item
import FNIStash.File.Variables
import FNIStash.File.General
import FNIStash.Logic.Env

import Control.Applicative
import Data.List.Utils
import Data.Binary.Put
import qualified Data.Text as T
import qualified Data.ByteString as BS

------ GENERAL STUFF

-- Given a Translate instance and a sentence full of markup (like This item gives [VALUE] to strength")
-- returns a translated sentence with all markup elements replaced with their translations.

type TranslateFxn =  String -> String

translateSentence :: TranslateFxn -> String -> String
translateSentence translateMarkup sent =
    let breakOn a = break (a ==)
        (pref, post1) = breakOn '[' sent
        (markup, post2) = breakOn ']' (drop 1 post1)
        suffix = drop 1 post2
        translatedMarkup = translateMarkup markup
        newSent = pref ++ translatedMarkup ++ suffix
    in if null post1 then sent -- nothing left to translate
       else translateSentence translateMarkup newSent




------ BASE ITEM STUFF

data Quality = Normal | Magic | Unique | UnknownQuality deriving (Eq, Ord)

data ItemClass = Axe2H | Mace2H | Sword2H | Axe1H | Bow | Cannon | Crossbow | Fist | Mace1H
               | Pistol | Polearm | Rifle | Shield | Staff | Sword1H | Wand | Necklace | Belt
               | Boots | ChestArmor | Collar | Gloves | Helmet | Pants | Ring | ShoulderArmor
               | Stud | Socketable | UnknownClass

data ItemBase = ItemBase
    { iBaseGUID :: ItemGUID
    , iBaseIcon :: FilePath
--    , iBaseQuality :: Quality
--    , iBaseClass :: ItemClass
--    , iBaseStrReq :: Maybe Int
--    , iBaseDexReq :: Maybe Int
--    , iBaseMagReq :: Maybe Int
--    , iBaseVitReq :: Maybe Int
--    , iBaseSpeed :: Maybe Int
--    , iBaseRange :: Maybe Float
--    , iBaseMaxSockets :: Maybe Int
--    , iBaseRarity :: Int
    } deriving (Eq, Ord)

searchAncestryFor (env@Env{..}) findMeVar itemDat =
    let foundVar = return itemDat >>= findMeVar
        itemBase = return itemDat >>= vBASEFILE
    in case foundVar of
        Just var -> foundVar -- we found the data we want!
        Nothing ->
            itemBase >>= lkupItemPath >>= searchAncestryFor env findMeVar

getItemBase (env@Env{..}) guid =
    let Just item = lkupItemGUID guid
        find k = searchAncestryFor env k item
        Just icon = find vICON
    in ItemBase guid icon





----- LOCATION STUFF

data Location = Location
    { locContainer :: String
    , locSlot :: String
    , locIndex :: Int
    }
    | Inserted
    | UnknownLocation
    deriving (Eq, Ord, Show)

-- Given the raw bytes for location in the item file, decode from the bytes to a
-- Location record that has 
decodeLocationBytes (Env {..}) (locBytes@LocationBytes {..})
    | lBytesSlotIndex == 0xFFFF && lBytesContainer == 0xFFFF = Inserted
    | otherwise = 
        let (slotType, container) = lkupLocNodes locBytes
            Just containerName = container >>= vNAME >>= return . T.unpack
            Just slotName = vNAME slotType >>= return . T.unpack
            slotID = vUNIQUEID slotType
            Just index = (-) lBytesSlotIndex <$> slotID
        in  Location containerName slotName (fromIntegral index)


encodeLocationBytes :: Env -> Location -> LocationBytes
encodeLocationBytes (Env {..}) loc = do
    let (Just slotID, Just contID) = lkupLocIDs (locSlot loc) (locContainer loc)
        slotIndex = (slotIDVal slotID + (fromIntegral $ locIndex loc))
    LocationBytes slotIndex (containerIDVal contID)




----- MODIFIER STUFF

data Mod = Mod
    { mDescription :: EffectDescription
    , mValue :: Float
    , mDisplayPrecision :: Int
    } deriving (Eq, Ord)

descTypeLookup 0x00 = GOODDES
descTypeLookup 0x01 = GOODDES
descTypeLookup 0x02 = GOODDESOT
descTypeLookup 0x03 = BADDES  -- This is just a guess
descTypeLookup 0x04 = BADDESOT -- This is also a guess
descTypeLookup _    = UnknownDescriptionType

effectDescription prec skill effectNode (eff@EffectBytes {..}) =
    let descType = case descTypeLookup eBytesDescriptionType of
            GOODDES     -> vGOODDES
            GOODDESOT   -> vGOODDESOT
            BADDES      -> vBADDES
            BADDESOT    -> vBADDESOT
            UnknownDescriptionType ->
                vUnknown (EffectDescription UnknownDescriptionType $
                    "!Effect (type, index): " ++ show (eBytesType, eBytesIndex))
        maybeSentence = (effectNode >>= descType)
    in case maybeSentence of
        Just (EffectDescription a sentence) -> EffectDescription a $
                            translateSentence (effectTranslator prec skill eff) sentence
        Nothing -> EffectDescription UnknownDescriptionType $
                    "!No description of " ++ show eBytesDescriptionType ++ " for index " ++ show eBytesIndex

effectPrecisionVal effectNode =
    let maybePrecision = effectNode >>= vDISPLAYPRECISION
    in maybe 1 id maybePrecision

showPrecision prec showable =
    let (preDecimal, postDecimal) = break (== '.') (show showable)
    in if prec == 0
        then preDecimal
        else preDecimal ++ (take (prec+1) postDecimal)


data DamageType = Physical | Fire | Electric | Ice | Poison | All | UnknownDamageType
    deriving (Ord, Show, Eq)

damageTypeLookup 0x00 = Physical
damageTypeLookup 0x02 = Fire
damageTypeLookup 0x03 = Ice
damageTypeLookup 0x04 = Electric
damageTypeLookup 0x05 = Poison
damageTypeLookup 0x06 = All
damageTypeLookup _ = UnknownDamageType

effectTranslator precVal maybeSkillName (eff@EffectBytes{..}) markup =
    let dispVal = showPrecision precVal
        fromList i = dispVal $ wordToFloat $ (flip (!!) i) eBytesValueList 
    in case markup of
        "VALUE"     -> "VALUE" -- leave VALUE unchanged so we can store in DB smarter
        "DURATION"  -> let val = dispVal (wordToFloat eBytesDuration)
                           suffix = if val == "1" then " second" else " seconds"
                       in val ++ suffix
        "DMGTYPE"   -> show (damageTypeLookup eBytesDamageType)
        "VALUE1"    -> fromList 1
        "VALUE2"    -> fromList 2
        "VALUE3"    -> fromList 3
        "VALUE4"    -> fromList 4
        "VALUE3AND4"-> fromList 3
        "VALUE_OT"  -> dispVal $ fromIntegral (ceiling $ wordToFloat eBytesValue)  * (wordToFloat eBytesDuration)
        "NAME"      -> maybe ("?Name?") id maybeSkillName
        _           -> "???"
            

decodeEffectBytes (Env{..}) (eff@EffectBytes {..}) =
    let effIndex = EffectIndex eBytesIndex
        effNode = lkupEffect effIndex
        value = wordToFloat eBytesValue
        precision = effectPrecisionVal effNode
        skillname = lkupSkill (T.pack eBytesName) >>= vDISPLAYNAME
        description = effectDescription precision skillname effNode eff
    in Mod description value precision


instance Show Mod where
    show (Mod{..}) =
        let nominal = replace "VALUE" (showPrecision mDisplayPrecision mValue) $ effDesc mDescription
        in case effDescType mDescription of
            GOODDES ->  nominal
            BADDES ->   nominal
            GOODDESOT -> "Conveys " ++ nominal
            BADDESOT ->  "Conveys " ++ nominal
            UnknownDescriptionType -> "!!" ++ nominal


----- FOR DEALING WITH POINT VALUES LIKE DAMAGE AND ARMOR
data PointValue = DamageVal Int | ArmorVal Int | NoVal deriving (Eq, Ord)

instance Show PointValue where
    show (DamageVal v) = show v ++ " damage"
    show (ArmorVal v)  = show v ++ " armor"
    show NoVal         = ""

---- COMPLETE ITEM STUFF

data Item = Item
    { iName :: String
    , iRandomID :: BS.ByteString
    , iIdentified :: Bool
    , iLocation :: Location
    , iLevel :: Int
    , iQuantity :: Int
    , iNumSockets :: Int
    , iGems :: [Item]
    , iPoints :: PointValue
    , iEffects :: [Mod]
    , iEnchantments :: [Mod]
    , iTriggerables :: [Mod]
    , iPartition :: Partition
    , iBase :: ItemBase
    } deriving (Eq, Ord)

getItem env bs = decodeItemBytes env <$> getItemBytes bs

decodeIdentified 0x00 = False
decodeIdentified _ = True

decodePoints 0xFFFFFFFF 0xFFFFFFFF = NoVal
decodePoints a 0xFFFFFFFF = DamageVal $ fromIntegral a
decodePoints 0xFFFFFFFF a = ArmorVal $ fromIntegral a

--data AddedDamage = AddedDamage
--decodeAddedDamage (AddedDamageBytes {..}) =

decodeItemBytes env (ItemBytes {..}) = Item
    iBytesName
    iBytesRandomID
    (decodeIdentified iBytesIdentified)
    (decodeLocationBytes env iBytesLocation)
    (fromIntegral iBytesLevel)
    (fromIntegral iBytesQuantity)
    (fromIntegral iBytesNumSockets)
    (map (decodeItemBytes env) iBytesGems)
    (decodePoints iBytesDamage iBytesArmor)
    (map (decodeEffectBytes env) (iBytesEffects ++ iBytesEffects2))
    []
    []
    iBytesPartition
    (getItemBase env (ItemGUID $ fromIntegral iBytesGUID))




putItem env (Item {..}) = putPartition (encodeLocationBytes env iLocation) iPartition
itemAsBS env item = runPut $ putItem env item









    




