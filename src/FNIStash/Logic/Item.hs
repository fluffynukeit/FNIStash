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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module FNIStash.Logic.Item
    ( getItem
    , putItem
    , Item(..)
    , ItemBase(..)
    , Location(..)
    , Partition(..)
    , Mod(..)
    , UnitType(..)
    , Quality(..)
    ) where

-- This file is for decodeing raw bytes of items into useables types and useable information

import FNIStash.File.Item
import FNIStash.File.Variables
import FNIStash.File.General
import FNIStash.Logic.Env

import Control.Applicative
import Data.List.Utils
import Data.Binary.Put
import Data.Maybe
import Data.Convertible
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.List as L

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

data Descriptor = Descriptor
    { descriptorString :: String
    , descriptorValue :: Float
    , descriptorPrec :: Int
    } deriving (Eq, Ord)

descriptorTranslator prec value "VALUE" = showPrecision prec value
descriptorTranslator prec value _       = "???"

instance Show Descriptor where
    show (Descriptor{..}) = translateSentence (descriptorTranslator descriptorPrec descriptorValue) descriptorString

data ItemBase = ItemBase
    { iBaseGUID :: ItemGUID
    , iBaseIcon :: FilePath
    , iBaseUnitType :: UnitType
    , iBaseOtherReqs :: [Descriptor]
    , iBaseLevelReq :: Descriptor
    , iBaseInnates :: [Descriptor]
    , iBaseRange :: Maybe Float
    , iBaseMaxSockets :: Maybe Int
    , iBaseRarity :: Maybe Int
    , iBaseDescription :: Maybe String
    } deriving (Eq, Ord)

searchAncestryFor (env@Env{..}) findMeVar itemDat =
    let foundVar = return itemDat >>= findMeVar
        itemBase = return itemDat >>= vBASEFILE
    in case foundVar of
        Just var -> foundVar -- we found the data we want!
        Nothing ->
            itemBase >>= lkupPath >>= searchAncestryFor env findMeVar

getItemBase (env@Env{..}) guid itemLevel =
    let Just item = lkupItemGUID guid
        find k = searchAncestryFor env k item
        Just icon = find vICON
        minDmg = find vMINDAMAGE >>= return . (resolveDmg env itemLevel) . fromIntegral >>= return . applyDmgMods
        maxDmg = find vMAXDAMAGE >>= return . (resolveDmg env itemLevel) . fromIntegral >>= return . applyDmgMods
        spdDmgMod = maybe (fromIntegral 1) id (find vSPEED_DMG_MOD)
        rareDmgMod = maybe (fromIntegral 1) id (find vRARITY_DMG_MOD)
        specDmgMod = maybe (fromIntegral 1) id (find vSPECIAL_DMG_MOD)
        dmgMods = [rareDmgMod, spdDmgMod, specDmgMod]
        applyDmgMods = \x -> fromIntegral . floor $ L.foldl (\acc mod -> acc * mod) x dmgMods
        unitType = fromJust $ find vUNITTYPE
    in ItemBase guid icon
        unitType

        -- Other reqs
        (catMaybes
        [ find vSTRENGTH_REQUIRED >>= return . mkStatReq . (resolveStat env itemLevel)
        , find vDEXTERITY_REQUIRED >>= return . mkStatReq . (resolveStat env itemLevel)
        , find vMAGIC_REQUIRED >>= return . mkStatReq . (resolveStat env itemLevel)
        , find vDEFENSE_REQUIRED >>= return . mkStatReq . (resolveStat env itemLevel)
        , find vREQ_CLASS >>= return . resolveClassReq
        ])

        -- Level req
        (mkLvlReq $ maybe (resolveLvlReq env itemLevel unitType) id (find vLEVEL_REQUIRED))

        -- Innate stuff
        (catMaybes
        [ find vSPEED >>= return . mkSpeed
        , vDAMAGE_PHYSICAL minDmg maxDmg item >>= return . mkDmg
        , vDAMAGE_POISON minDmg maxDmg item >>= return . mkDmg
        , vDAMAGE_ELECTRIC minDmg maxDmg item >>= return . mkDmg
        , vDAMAGE_ICE minDmg maxDmg item >>= return . mkDmg
        , vDAMAGE_FIRE minDmg maxDmg item >>= return . mkDmg
        ])
        (find vRANGE)
        (find vMAX_SOCKETS)
        (find vRARITY)
        (find vDESCRIPTION)

-- Stat calculations

resolveStat (Env{..}) itemLevel (StatReq stat val) =
    let interp = case stat of
            Strength -> lkupGraph "MEDIA/GRAPHS/STATS/ITEM_STRENGTH_REQUIREMENTS.DAT" (fromIntegral itemLevel)
            Dexterity-> lkupGraph "MEDIA/GRAPHS/STATS/ITEM_DEXTERITY_REQUIREMENTS.DAT" (fromIntegral itemLevel)
            Focus    -> lkupGraph "MEDIA/GRAPHS/STATS/ITEM_MAGIC_REQUIREMENTS.DAT" (fromIntegral itemLevel)
            Vitality -> lkupGraph "MEDIA/GRAPHS/STATS/ITEM_DEFENSE_REQUIREMENTS.DAT" (fromIntegral itemLevel)
    in StatReq stat (floor $ fromIntegral val * interp/100)

mkStatReq (StatReq stat val) = Descriptor ("Requires " ++ show stat ++ " " ++ "[VALUE]") (fromIntegral val) 0
mkSpeed speed | speed < 0.8 = Descriptor "Very Fast Attack Speed ([VALUE] seconds)" speed 2
              | speed < 0.96 = Descriptor "Fast Attack Speed ([VALUE] seconds)" speed 2
              | speed < 1.04 = Descriptor "Average Attack Speed ([VALUE] seconds)" speed 2
              | speed <= 1.2 = Descriptor "Slow Attack Speed ([VALUE] seconds)" speed 2
              | otherwise    = Descriptor "Very Slow Attack Speed ([VALUE] seconds)" speed 2

-- Damage calculations

resolveDmg (Env{..}) itemLevel dmgVal =
    let multiplierPercent = lkupGraph "MEDIA/GRAPHS/STATS/BASE_WEAPON_DAMAGE.DAT" (fromIntegral itemLevel)
    in dmgVal * multiplierPercent/100

mkDmg (Damage dType low high) =
    Descriptor (show dType ++ " Damage: " ++ "[VALUE]-" ++ (showPrecision 0 high))
    low 0

-- Lvl Requirement

resolveLvlReq (Env{..}) itemLevel (UnitType {..})
    | uType == "SOCKETABLE" = floor $ lkupGraph "MEDIA/GRAPHS/STATS/ITEM_LEVEL_REQUIREMENTS_SOCKETABLE.DAT" $ fromIntegral itemLevel
    | uQuality /= NormalQ && uQuality /= NoneQ = floor $ lkupGraph "MEDIA/GRAPHS/STATS/ITEM_LEVEL_REQUIREMENTS.DAT" $ fromIntegral itemLevel
    | otherwise = floor $ lkupGraph "MEDIA/GRAPHS/STATS/ITEM_LEVEL_REQUIREMENTS_NORMAL.DAT" $ fromIntegral itemLevel

mkLvlReq i = Descriptor "Requires Level [VALUE]" (fromIntegral i) 0

-- Class requirement
resolveClassReq (uType -> "RAILMAN")   = Descriptor "Requires Class: Engineer" 0 0
resolveClassReq (uType -> "OUTLANDER") = Descriptor "Requires Class: Outlander" 0 0
resolveClassReq (uType -> "BERSERKER") = Descriptor "Requires Class: Berserker" 0 0
resolveClassReq (uType -> "EMBERMAGE") = Descriptor "Requires Class: Embermage" 0 0
resolveClassReq _                      = Descriptor "Requires Class: Unknown Class" 0 0

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
    { mIsPossibleEnchant :: Bool
    , mDescription :: EffectDescription
    , mValue :: Float
    , mDisplayPrecision :: Int
    } deriving (Eq, Ord)
--
--isEnchantment (Mod Enchantment _ _ _) = True
--isEnchantment (Mod _ _ _ _) = False
--
--isEffect (Mod Effect _ _ _) = True
--isEffect (Mod _ _ _ _) = False

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

roundAt prec val = ((fromIntegral . ceiling) (val*10^prec)) / 10^prec

showPrecision prec showable =
    let (preDecimal, postDecimal) = break (== '.') (show . roundAt prec $ showable)
    in if prec == 0
        then preDecimal
        else preDecimal ++ (take (prec+1) postDecimal)

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
        "VALUE_OT"  -> dispVal $ (roundAt precVal $ wordToFloat eBytesValue)  * (wordToFloat eBytesDuration)
        "NAME"      -> maybe ("?Name?") id maybeSkillName
        _           -> "???"
            

decodeEffectBytes (Env{..}) (eff@EffectBytes {..}) =
    let effIndex = EffectIndex eBytesIndex
        effNode = lkupEffect effIndex
        value = wordToFloat eBytesValue
        precision = effectPrecisionVal effNode
        skillname = lkupSkill (T.pack eBytesName) >>= vDISPLAYNAME
        description = effectDescription precision skillname effNode eff
        mType = null eBytesName
    in Mod mType description value precision

instance Convertible AddedDamageBytes EffectBytes where
    safeConvert (AddedDamageBytes{..}) = Right $
        EffectBytes 0 "" Nothing Nothing 0 [] 0x0a dBytesDamageType 0 0 0 dBytesFromEnchant 0 Nothing



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

decodeItemBytes env (ItemBytes {..}) =
    let allMods = (map (decodeEffectBytes env) (iBytesEffects ++ iBytesEffects2))
        enchantAdditions = filter ((/=) 0 . dBytesFromEnchant) iBytesAddedDamages
        (normalMods, enchantMods) = L.partition (not.mIsPossibleEnchant) allMods
        numEnchMods = fromIntegral iBytesNumEnchants - (length enchantAdditions)
        (useEnchantsEffects, backToNormal) = L.splitAt numEnchMods enchantMods
        useNormal = backToNormal ++ normalMods
        convertedAdditions = map (decodeEffectBytes env . convert) enchantAdditions
        useEnchants = convertedAdditions ++ useEnchantsEffects
    in
        Item
        iBytesName
        iBytesRandomID
        (decodeIdentified iBytesIdentified)
        (decodeLocationBytes env iBytesLocation)
        (fromIntegral iBytesLevel)
        (fromIntegral iBytesQuantity)
        (fromIntegral iBytesNumSockets)
        (map (decodeItemBytes env) iBytesGems)
        (decodePoints iBytesDamage iBytesArmor)
        useNormal
        useEnchants
        []
        iBytesPartition
        (getItemBase env (ItemGUID $ fromIntegral iBytesGUID) iBytesLevel)




putItem env (Item {..}) = putPartition (encodeLocationBytes env iLocation) iPartition
itemAsBS env item = runPut $ putItem env item









    




