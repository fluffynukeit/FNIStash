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
    , PointValue(..)
    ) where

-- This file is for decodeing raw bytes of items into useables types and useable information

import FNIStash.File.Item
import FNIStash.File.Variables
import FNIStash.File.General
import FNIStash.File.DAT
import FNIStash.Logic.Env

import Control.Applicative
import Data.List.Utils
import Data.Binary.Put
import Data.Maybe
import Data.Convertible
import Data.Bits
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
        newSent = pref ++ translatedMarkup
    in if null post1 then sent -- nothing left to translate
       else newSent ++ translateSentence translateMarkup suffix




------ BASE ITEM STUFF

data Descriptor = Descriptor
    { descriptorString :: String
    , descriptorValue :: Float
    , descriptorPrec :: Int
    } deriving (Eq, Ord)

descriptorTranslator prec value "*" = showPrecision prec value
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
    , iBaseDescription :: Maybe [Descriptor] -- 1 descriptor per line of description
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
        unitType = fromJust $ find vITEMUNITTYPE
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
        (find vDESCRIPTION >>= return . resolveDesc)

-- Stat calculations

resolveStat (Env{..}) itemLevel (StatReq stat val) =
    let interp = case stat of
            Strength -> lkupGraph "MEDIA/GRAPHS/STATS/ITEM_STRENGTH_REQUIREMENTS.DAT" (fromIntegral itemLevel)
            Dexterity-> lkupGraph "MEDIA/GRAPHS/STATS/ITEM_DEXTERITY_REQUIREMENTS.DAT" (fromIntegral itemLevel)
            Focus    -> lkupGraph "MEDIA/GRAPHS/STATS/ITEM_MAGIC_REQUIREMENTS.DAT" (fromIntegral itemLevel)
            Vitality -> lkupGraph "MEDIA/GRAPHS/STATS/ITEM_DEFENSE_REQUIREMENTS.DAT" (fromIntegral itemLevel)
    in StatReq stat (floor $ fromIntegral val * interp/100)

mkStatReq (StatReq stat val) = Descriptor ("Requires " ++ show stat ++ " " ++ "[*]") (fromIntegral val) 0
mkSpeed speed | speed < 0.8 = Descriptor "Very Fast Attack Speed ([*] seconds)" speed 2
              | speed < 0.96 = Descriptor "Fast Attack Speed ([*] seconds)" speed 2
              | speed < 1.04 = Descriptor "Average Attack Speed ([*] seconds)" speed 2
              | speed <= 1.2 = Descriptor "Slow Attack Speed ([*] seconds)" speed 2
              | otherwise    = Descriptor "Very Slow Attack Speed ([*] seconds)" speed 2

-- Damage calculations

resolveDmg (Env{..}) itemLevel dmgVal =
    let multiplierPercent = lkupGraph "MEDIA/GRAPHS/STATS/BASE_WEAPON_DAMAGE.DAT" (fromIntegral itemLevel)
    in dmgVal * multiplierPercent/100

mkDmg (Damage dType low high) =
    Descriptor (show dType ++ " Damage: " ++ "[*]-" ++ (showPrecision 0 high))
    low 0

-- Lvl Requirement

resolveLvlReq (Env{..}) itemLevel (UnitType {..})
    | uType == "SOCKETABLE" = floor $ lkupGraph "MEDIA/GRAPHS/STATS/ITEM_LEVEL_REQUIREMENTS_SOCKETABLE.DAT" $ fromIntegral itemLevel
    | uQuality /= NormalQ && uQuality /= NoneQ = floor $ lkupGraph "MEDIA/GRAPHS/STATS/ITEM_LEVEL_REQUIREMENTS.DAT" $ fromIntegral itemLevel
    | otherwise = floor $ lkupGraph "MEDIA/GRAPHS/STATS/ITEM_LEVEL_REQUIREMENTS_NORMAL.DAT" $ fromIntegral itemLevel

mkLvlReq i = Descriptor "Requires Level [*]" (fromIntegral i) 0

-- Class requirement
resolveClassReq "RAILMAN"   = Descriptor "Requires Class: Engineer" 0 0
resolveClassReq "OUTLANDER" = Descriptor "Requires Class: Outlander" 0 0
resolveClassReq "BERSERKER" = Descriptor "Requires Class: Berserker" 0 0
resolveClassReq "EMBERMAGE" = Descriptor "Requires Class: Embermage" 0 0
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
    { mIsEnchant :: Bool
    , mDescriptor :: Descriptor
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
        "VALUE"     -> "[*]" -- leave VALUE unchanged so we can store in DB smarter
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
        ench = isEnchant eff
    in Mod ench $ Descriptor (mkModDescriptor description) value precision

mkModDescriptor (EffectDescription {..}) =
    let nominal = effDesc
    in case effDescType of
        GOODDES ->  nominal
        BADDES ->   nominal
        GOODDESOT -> "Conveys " ++ nominal
        BADDESOT ->  "Conveys " ++ nominal
        UnknownDescriptionType -> "!!" ++ nominal


isEnchant (EffectBytes{..}) =
    let byteTest = eBytesType .&. 0x0000FF00
        notEnchant = [0x8000, 0x8100, 0xA000] -- list of bytes for nameless skills but NOT enchants
    in null eBytesName && all (byteTest /=) notEnchant

instance Convertible AddedDamageBytes EffectBytes where
    safeConvert (AddedDamageBytes{..}) = Right $
        EffectBytes 0 "" Nothing Nothing 0 [] 0x0a dBytesDamageType 0 0 0 dBytesFromEnchant 0 Nothing


----- FOR DEALING WITH POINT VALUES LIKE DAMAGE AND ARMOR
data PointValue = DamageVal Int | ArmorVal Int | NoVal deriving (Eq, Ord)

instance Show PointValue where
    show (DamageVal v) = show v ++ " damage"
    show (ArmorVal v)  = show v ++ " armor"
    show NoVal         = ""


-- For dealing with description strings

resolveDesc = map (\s -> Descriptor s 0 0 ) . lines . fixNewLines
    where fixNewLines = replace "\\n" "\n"


-- For getting description of gem effects

getGemDesc (env@Env{..}) (Item{..}) =
    let title = Descriptor iName 0 0
        icon = iBaseIcon iBase
        effect = iEffectsRaw !! 0
    in (icon, title, effect)



---- COMPLETE ITEM STUFF

data Item = Item
    { iName :: String
    , iRandomID :: BS.ByteString
    , iIdentified :: Bool
    , iLocation :: Location
    , iLevel :: Int
    , iQuantity :: Int
    , iNumSockets :: Int
    , iGems :: [(FilePath, Descriptor, Descriptor)] -- (icon, title, desc) triplet for each gem
    , iEffectsRaw :: [Descriptor]
    , iEffects :: [Descriptor]
    , iEnchantments :: [Descriptor]
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

selectModEnchants env (ItemBytes{..}) =
    let allMods = (map (decodeEffectBytes env) (iBytesEffects ++ iBytesEffects2))
        enchantAdditions = filter ((/=) 0 . dBytesFromEnchant) iBytesAddedDamages
        (enchantMods, normalMods) = L.partition (mIsEnchant) allMods

        convertedAdditions = map (decodeEffectBytes env . convert) enchantAdditions
        enchantLabel = if iBytesNumEnchants > 0
            then [Descriptor "Enchantments: [*]" (fromIntegral iBytesNumEnchants) 0]
            else []
        useEnchants = enchantLabel ++ (map mDescriptor $ convertedAdditions ++ enchantMods)
        useNormal = map mDescriptor normalMods
    in (useNormal, useEnchants)


maybeToBool Nothing = False
maybeToBool (Just b)  = b

-- This function is mostly used for determining which of two effects on a gem belongs to armor and
-- which belongs to weapons.  It's not consistent even among unique gems, but might? be consistent
-- among normal gems?
makeGemDescriptors env (iEffectsRaw -> []) effIndexList = []
makeGemDescriptors (env@Env{..}) (Item {..}) effIndexList =
    let gemDat = lkupItemGUID (iBaseGUID iBase)
        effAtIndex = lkupEffect . EffectIndex . ((!!) effIndexList)
        eff0 = effAtIndex 0
        eff1 = effAtIndex 1
        checkAgainstEff x | x == eff0 = Just 0
                          | x == eff1 = Just 1
                          | otherwise = Nothing
        tryFirst a@(Just c) b = a
        tryFirst Nothing    b = b
        checkValidLength ind = if ind < length iEffectsRaw then Just ind else Nothing

        -- This is the code for getting effect from a unique socketable
        getMatchingTypeBy key d = nEFFECT d >>= vTYPE >>= return . lkupEffect . EffectName
        aAffix = gemDat >>= vAFFIX_A >>= lkupAffix . AffixName
        bAffix = gemDat >>= vAFFIX_B >>= lkupAffix . AffixName

        hasType searchType d = maybeToBool $ d >>= nUNITTYPES >>=
            return . any (\(_, var) -> searchType == (fromJust $ stringVar var)) . datNodeVars

        armorAffix  = if hasType "ARMOR"  aAffix then aAffix else bAffix
        weaponAffix = if hasType "WEAPON" aAffix then aAffix else bAffix

        armorEffect  = armorAffix  >>= getMatchingTypeBy AffixName
        weaponEffect = weaponAffix >>= getMatchingTypeBy AffixName

        armorIndex  = armorEffect  >>= checkAgainstEff >>= checkValidLength
        weaponIndex = weaponEffect >>= checkAgainstEff >>= checkValidLength

        -- Here is the description for a normal gem
        normalArmorInd  = return 1 >>= checkValidLength
        normalWeaponInd = return 0 >>= checkValidLength

        armorDescriptor  = fromJust $ tryFirst armorIndex  normalArmorInd  >>= return . (!!) iEffectsRaw
        weaponDescriptor = fromJust $ tryFirst weaponIndex normalWeaponInd >>= return . (!!) iEffectsRaw
        armorSpecifier  = Descriptor "Armor/Trinkets:" 0 0
        weaponSpecifier = Descriptor "Weapons:" 0 0

        -- Figure out if our item is even a gem or not
        itemType = uType (iBaseUnitType iBase)
        socketItemTypes = ["SOCKETABLE", "BLOOD EMBER", "VOID EMBER", "CHAOS EMBER", "IRON EMBER"]
        isSocket = any (itemType ==) socketItemTypes
    in if not isSocket then iEffectsRaw -- No change to descriptors
       else armorSpecifier:armorDescriptor:weaponSpecifier:weaponDescriptor:[]

effectsOf item = iBytesEffects item ++ iBytesEffects2 item

decodeItemBytes env (itemBytes@ItemBytes {..}) =
    let (useNormal, useEnchants) = selectModEnchants env itemBytes
        gemItems = map (decodeItemBytes env) iBytesGems
        base = getItemBase env (ItemGUID $ fromIntegral iBytesGUID) iBytesLevel

        effectIndexList = map (eBytesIndex) $ effectsOf itemBytes

        item = Item
               iBytesName
               iBytesRandomID
               (decodeIdentified iBytesIdentified)
               (decodeLocationBytes env iBytesLocation)
               (fromIntegral iBytesLevel)
               (fromIntegral iBytesQuantity)
               (fromIntegral iBytesNumSockets)
               (map (getGemDesc env) $ gemItems )
               useNormal
               (makeGemDescriptors env item effectIndexList)
               useEnchants
               []
               iBytesPartition
               base
    in item



putItem env (Item {..}) = putPartition (encodeLocationBytes env iLocation) iPartition
itemAsBS env item = runPut $ putItem env item









    




