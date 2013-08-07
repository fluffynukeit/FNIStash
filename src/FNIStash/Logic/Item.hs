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
    , allDescriptorsOf
    , encodeLocationBytes
    , Item(..)
    , ItemBase(..)
    , Location(..)
    , Partition(..)
    , Mod(..)
    , UnitType(..)
    , Quality(..)
    , PointValue(..)
    , Descriptor(..)
    , ItemClass(..)
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
import Data.Binary.Strict.Get
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.List as L


import Debug.Trace

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

data ItemClass = Arms | Consumables | Spells deriving (Eq, Show, Ord)

data Descriptor = Descriptor
    { descriptorString :: String
    , descriptorValue :: Float
    , descriptorPrec :: Int
    } deriving (Eq, Ord)

descriptorTranslator prec value "*" = showPrecision prec value
descriptorTranslator prec value _       = "???"

findSublistIndex xss xs = L.findIndex (L.isPrefixOf xss) $ L.tails xs

mkDescriptor a =
    -- do some preprocessing on the description to remove unwanted characters
    let noU = replace "|u" "" a
        resolve k = maybe k (\i -> resolve $ take i k ++ drop (i+10) k) (findSublistIndex "|c" k)
    in Descriptor $ resolve noU

-- For dealing with description strings

fixNewLineDesc = map (\s -> mkDescriptor s 0 0 ) . lines . fixNewLines
    where fixNewLines = replace "\\n" "\n"


instance Show Descriptor where
    show (Descriptor{..}) = translateSentence (descriptorTranslator descriptorPrec descriptorValue) descriptorString

data ItemBase = ItemBase
    { iBaseGUID :: GUID
    , iBaseIcon :: FilePath
    , iBaseUnitType :: UnitType
    , iBaseOtherReqs :: [Descriptor]
    , iBaseLevelReq :: Descriptor
    , iBaseInnates :: [Descriptor]
    , iBaseRange :: Maybe Float
    , iBaseMaxSockets :: Maybe Int
    , iBaseRarity :: Maybe Int
    , iBaseDescription :: [Descriptor] -- 1 descriptor per line of description
    } deriving (Eq, Ord)


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
        (maybe [] id $ find vDESCRIPTION >>= return . fixNewLineDesc)

-- Stat calculations

resolveStat (Env{..}) itemLevel (StatReq stat val) =
    let interp = case stat of
            Strength -> lkupGraph "MEDIA/GRAPHS/STATS/ITEM_STRENGTH_REQUIREMENTS.DAT" (fromIntegral itemLevel)
            Dexterity-> lkupGraph "MEDIA/GRAPHS/STATS/ITEM_DEXTERITY_REQUIREMENTS.DAT" (fromIntegral itemLevel)
            Focus    -> lkupGraph "MEDIA/GRAPHS/STATS/ITEM_MAGIC_REQUIREMENTS.DAT" (fromIntegral itemLevel)
            Vitality -> lkupGraph "MEDIA/GRAPHS/STATS/ITEM_DEFENSE_REQUIREMENTS.DAT" (fromIntegral itemLevel)
    in StatReq stat (floor $ fromIntegral val * interp/100)

mkStatReq (StatReq stat val) = mkDescriptor ("Requires " ++ show stat ++ " " ++ "[*]") (fromIntegral val) 0
mkSpeed speed | speed < 0.8 = mkDescriptor "Very Fast Attack Speed ([*] seconds)" speed 2
              | speed < 0.96 = mkDescriptor "Fast Attack Speed ([*] seconds)" speed 2
              | speed < 1.04 = mkDescriptor "Average Attack Speed ([*] seconds)" speed 2
              | speed <= 1.2 = mkDescriptor "Slow Attack Speed ([*] seconds)" speed 2
              | otherwise    = mkDescriptor "Very Slow Attack Speed ([*] seconds)" speed 2

-- Damage calculations

resolveDmg (Env{..}) itemLevel dmgVal =
    let multiplierPercent = lkupGraph "MEDIA/GRAPHS/STATS/BASE_WEAPON_DAMAGE.DAT" (fromIntegral itemLevel)
    in dmgVal * multiplierPercent/100

mkDmg (Damage dType low high) =
    mkDescriptor (show dType ++ " Damage: " ++ "[*]-" ++ (showPrecision 0 high))
    low 0

-- Lvl Requirement

resolveLvlReq (Env{..}) itemLevel (UnitType {..})
    | uType == "SOCKETABLE" = floor $ lkupGraph "MEDIA/GRAPHS/STATS/ITEM_LEVEL_REQUIREMENTS_SOCKETABLE.DAT" $ fromIntegral itemLevel
    | uQuality /= NormalQ && uQuality /= NoneQ = floor $ lkupGraph "MEDIA/GRAPHS/STATS/ITEM_LEVEL_REQUIREMENTS.DAT" $ fromIntegral itemLevel
    | otherwise = floor $ lkupGraph "MEDIA/GRAPHS/STATS/ITEM_LEVEL_REQUIREMENTS_NORMAL.DAT" $ fromIntegral itemLevel

mkLvlReq i = mkDescriptor "Requires Level [*]" (fromIntegral i) 0

-- Class requirement
resolveClassReq "RAILMAN"   = mkDescriptor "Requires Class: Engineer" 0 0
resolveClassReq "OUTLANDER" = mkDescriptor "Requires Class: Outlander" 0 0
resolveClassReq "BERSERKER" = mkDescriptor "Requires Class: Berserker" 0 0
resolveClassReq "EMBERMAGE" = mkDescriptor "Requires Class: Embermage" 0 0
resolveClassReq _           = mkDescriptor "Requires Class: Unknown Class" 0 0

----- LOCATION STUFF

data Location = Location
    { locContainer :: String
    , locSlot :: String
    , locIndex :: Int
    }
    | InsertedInSocket
    | Archive {rowID :: Int}
    | UnknownLocation
    deriving (Eq, Ord, Show)

-- Given the raw bytes for location in the item file, decode from the bytes to a
-- Location record that has 
decodeLocationBytes (Env {..}) (locBytes@LocationBytes {..})
    | lBytesSlotIndex == 0xFFFF && lBytesContainer == 0xFFFF = InsertedInSocket
    | otherwise = 
        let (slotType, container) = lkupLocNodes locBytes
            Just containerName = container >>= vNAME >>= return . T.unpack
            Just slotName = vNAME slotType >>= return . T.unpack
            slotID = vUNIQUEID slotType
            Just index = (-) lBytesSlotIndex <$> slotID
        in  Location containerName slotName (fromIntegral index)


encodeLocationBytes :: Env -> Location -> LocationBytes
encodeLocationBytes Env{..} loc@Location{..} = 
    let
        (Just slotID, Just contID) = lkupLocIDs locSlot locContainer
        slotIndex = (slotIDVal slotID + (fromIntegral locIndex))
    in LocationBytes slotIndex (containerIDVal contID)




----- MODIFIER STUFF

data Mod = Mod
    { mIsEnchant :: Bool
    , mEffectName :: T.Text
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

effectDescription prec maybeName effectNode (eff@EffectBytes {..}) =
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
                            translateSentence (effectTranslator prec maybeName eff) sentence
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

effectTranslator precVal maybeName (eff@EffectBytes{..}) markup =
    let dispVal = showPrecision precVal
        fromList i = dispVal $ wordToFloat $ (flip (!!) i) eBytesValueList
        makeDurationString valStr = (++) valStr (if valStr == "1" then " second" else " seconds")
    in case markup of
        "VALUE"     -> "[*]" -- leave VALUE unchanged so we can store in DB smarter
        "DURATION"  -> makeDurationString $ dispVal (wordToFloat eBytesDuration)
        "DMGTYPE"   -> show (damageTypeLookup eBytesDamageType)
        "VALUE1"    -> fromList 1
        "VALUE2"    -> fromList 2
        "VALUE3"    -> fromList 3
        "VALUE4"    -> fromList 4
        "VALUE3AND4"-> fromList 3
        "VALUE_OT"  -> dispVal $ (roundAt precVal $ wordToFloat eBytesValue)  * (wordToFloat eBytesDuration)
        "NAME"      -> maybe ("?Name?") id maybeName
        "VALUE1ASDURATION" -> makeDurationString $ fromList 1
        _           -> "???"
            

decodeEffectBytes (Env{..}) (eff@EffectBytes {..}) =
    let effIndex = EffectIndex eBytesIndex
        effNode = lkupEffect effIndex
        value = wordToFloat eBytesValue
        precision = effectPrecisionVal effNode
        skillname = lkupSkill $ T.pack eBytesName
        monstername = lkupMonster $ T.pack eBytesName
        nameToUse = (skillname <|> monstername) >>= vDISPLAYNAME
        description = effectDescription precision nameToUse effNode eff
        ench = isEnchant eff
        effName = fromJust $ effNode >>= vNAME
    in Mod ench effName $ mkDescriptor (mkModDescriptor description) value precision

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
    in (null eBytesName && all (byteTest /=) notEnchant) || eBytesType == 0x8541

instance Convertible AddedDamageBytes EffectBytes where
    safeConvert (AddedDamageBytes{..}) = Right $
        EffectBytes 0 "" Nothing Nothing 0 [] 0x0a dBytesDamageType 0 0 0 dBytesFromEnchant 0 Nothing


----- FOR DEALING WITH POINT VALUES LIKE DAMAGE AND ARMOR
data PointValue = DamageVal Int | ArmorVal Int | NoVal deriving (Eq, Ord)

instance Show PointValue where
    show (DamageVal v) = show v ++ " damage"
    show (ArmorVal v)  = show v ++ " armor"
    show NoVal         = ""


-- For getting description of gem effects

getGemDesc (env@Env{..}) (Item{..}) =
    let title = iName
        icon = iBaseIcon iBase
        effect = iEffectsRaw !! 0
    in (icon, title, effect)



---- COMPLETE ITEM STUFF

data Item = Item
    { iName :: Descriptor
    , iRandomID :: BS.ByteString
    , iIdentified :: Bool
    , iLocation :: Location
    , iLevel :: Descriptor
    , iQuantity :: Int
    , iNumSockets :: Int
    , iGems :: [(FilePath, Descriptor, Descriptor)] -- (icon, title, desc) triplet for each gem
    , iGemsAsItems :: [Item]
    , iInnateDefs :: [(FilePath, Descriptor)]
    , iEffectsRaw :: [Descriptor]
    , iEffects :: [Descriptor]
    , iEnchantments :: [Descriptor]
    , iTriggerables :: [Descriptor]
    , iPartition :: Partition
    , iBase :: ItemBase
    , iID :: Maybe Int
    } deriving (Eq, Ord)

instance Show Item where
    show = show . iName

getItem env id bs = decodeItemBytes env id <$> getItemBytes bs

decodeIdentified 0x00 = False
decodeIdentified _ = True

decodePoints 0xFFFFFFFF 0xFFFFFFFF = NoVal
decodePoints a 0xFFFFFFFF = DamageVal $ fromIntegral a
decodePoints 0xFFFFFFFF a = ArmorVal $ fromIntegral a

--data AddedDamage = AddedDamage
--decodeAddedDamage (AddedDamageBytes {..}) =

selectSpecialEffects env (ItemBytes{..}) =
    let allMods = map (decodeEffectBytes env) (iBytesEffects ++ iBytesEffects2)
        enchantAdditions = filter ((/=) 0 . dBytesFromEnchant) iBytesAddedDamages
        (enchantMods, normalMods) = L.partition (mIsEnchant) allMods

        -- First determine enchants

        convertedAdditions = map (decodeEffectBytes env . convert) enchantAdditions
        enchantLabel = if iBytesNumEnchants > 0
            then [mkDescriptor "Enchantments: [*]" (fromIntegral iBytesNumEnchants) 0]
            else []
        useEnchants = enchantLabel ++ (map mDescriptor $ convertedAdditions ++ enchantMods)

        -- Out of remaining effects, find those with innate defense
        innateDefs = [ "INNATE ICE DEFENSE", "INNATE FIRE DEFENSE"
                     , "INNATE POISON DEFENSE", "INNATE ELECTRIC DEFENSE"]

        (innates, nonInnates) = L.partition (flip elem innateDefs . mEffectName) normalMods
        useNormal = map (mDescriptor) nonInnates

        -- assign the correct icons to innate defense
        innateDefDescriptors = map (mDescriptor) innates
        fileLookup "INNATE ICE DEFENSE"     = "resist_iced"
        fileLookup "INNATE ELECTRIC DEFENSE" = "resist_electricc"
        fileLookup "INNATE POISON DEFENSE"  = "resist_poisonc"
        fileLookup "INNATE FIRE DEFENSE"    = "resist_firec"

        defenseFiles = map (fileLookup . mEffectName) innates
        useInnateDef = zip defenseFiles innateDefDescriptors

        armor = case decodePoints iBytesDamage iBytesArmor of
                    ArmorVal 0 -> Nothing
                    ArmorVal i -> Just ("resist_physicalc", mkDescriptor "[*] Physical Armor" (fromIntegral i) 0)
                    _          -> Nothing
                
    in (useNormal, useEnchants, maybe [] (:[]) armor ++ useInnateDef)


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

        checkValidLength ind = if ind < length iEffectsRaw
            then Just ind
            else traceShow ("=========== Exceeded!!!", ind, length iEffectsRaw, iName) $ Nothing

        -- This is the code for getting effect from a unique socketable
        getMatchingTypeBy d = nEFFECT d >>= vTYPE >>= return . lkupEffect . EffectName
        aAffix = gemDat >>= vAFFIX_A >>= lkupAffix
        bAffix = gemDat >>= vAFFIX_B >>= lkupAffix

        hasType searchType d = maybeToBool $ d >>= nUNITTYPES >>=
            return . any (\(_, var) -> searchType == (fromJust $ stringVar var)) . datNodeVars

        armorAffix  = if hasType "ARMOR"  aAffix then aAffix else bAffix
        weaponAffix = if hasType "WEAPON" aAffix then aAffix else bAffix

        armorEffect  = armorAffix  >>= getMatchingTypeBy
        weaponEffect = weaponAffix >>= getMatchingTypeBy

        armorIndex  = armorEffect  >>= checkAgainstEff >>= checkValidLength
        weaponIndex = weaponEffect >>= checkAgainstEff >>= checkValidLength

        -- Here is the description for a normal gem
        normalArmorInd  = return 1 >>= checkValidLength
        normalWeaponInd = return 0 >>= checkValidLength

        armorDescriptor  = maybe (mkDescriptor "!!! BROKEN ARMOR" 0 0) id $
             (armorIndex <|> normalArmorInd)  >>= return . (!!) iEffectsRaw
        weaponDescriptor =  maybe (mkDescriptor "!!! BROKEN WEAPON" 0 0) id $
             (weaponIndex <|> normalWeaponInd) >>= return . (!!) iEffectsRaw
        armorSpecifier  = mkDescriptor "Armor/Trinkets:" 0 0
        weaponSpecifier = mkDescriptor "Weapons:" 0 0

        -- Figure out if our item is even a gem or not
        itemType = uType (iBaseUnitType iBase)
        socketItemTypes = ["SOCKETABLE", "BLOOD EMBER", "VOID EMBER", "CHAOS EMBER", "IRON EMBER"]
        isSocket = any (itemType ==) socketItemTypes
    in if not isSocket then iEffectsRaw -- No change to descriptors if not a gem or quest item
       else armorSpecifier:armorDescriptor:weaponSpecifier:weaponDescriptor:[]


getDescrip (Env {..}) (bytes@ItemBytes{..}) (TriggerableBytes name) =
    lkupTriggerable (T.pack name) >>= vDESCRIPTION >>= return . fixNewLineDesc . replaceKeywords
    where f = L.elemIndex
          extract a b = drop (a+1) . take b
          replaceKeywords k =
            let open = f '<' k; close = f '>' k;
                colon = L.filter (\i -> i > fromJust open && i < fromJust close) $ L.elemIndices ':' k;
            in case (open, colon, close) of
                    (Just op, co:[], Just cl) ->
                        let typeStr = extract op co k
                            keyStr  = extract co cl k
                            replacement = case typeStr of
                                "stat" -> getMatchingStatString bytes $ lkupStat (T.pack keyStr)
                                _      -> "!!Unknown stat type"
                        in take op k ++ replacement ++ drop (cl+1) k
                    _ -> k

getMatchingStatString (ItemBytes{..}) datFile =
    let statType = datFile >>= vTYPE
        guid = datFile >>= vUNIQUE_GUID
        guidMatch = guid >>= \g -> L.find ((==) g . fromIntegral . statGUID) iBytesStats
        matchingString = case (statType, guidMatch) of
            (Just "TYPE_INT", Just (StatBytes _ bs)) -> show $
                runGetSuppress (getWord32le >>= return . fromIntegral ::Get Int) bs
            _ -> "0"
    in matchingString



-- Make triggerable descriptors
makeTrigDescriptors env (bytes@ItemBytes{..}) = concat $ mapMaybe (getDescrip env bytes) iBytesTriggerables


effectsOf item = iBytesEffects item ++ iBytesEffects2 item

decodeItemBytes env id (itemBytes@ItemBytes {..}) =
    let (useNormal, useEnchants, innateDef) = selectSpecialEffects env itemBytes
        gemItems = map (decodeItemBytes env Nothing) iBytesGems
        base = getItemBase env (GUID $ fromIntegral iBytesGUID) iBytesLevel

        effectIndexList = map (eBytesIndex) $ effectsOf itemBytes
        item = Item
               (mkDescriptor iBytesName 0 0)
               iBytesRandomID
               (decodeIdentified iBytesIdentified)
               (decodeLocationBytes env iBytesLocation)
               (Descriptor "Level [*]" (fromIntegral iBytesLevel) 0)
               (fromIntegral iBytesQuantity)
               (fromIntegral iBytesNumSockets)
               (map (getGemDesc env) $ gemItems )
               gemItems
               innateDef
               useNormal
               (makeGemDescriptors env item effectIndexList)
               useEnchants
               (makeTrigDescriptors env itemBytes)
               iBytesPartition
               base
               id
    in item


-- Returns a list of all descriptors for the item.  The Name is not included in
-- in the returned list
allDescriptorsOf (Item{..}) =
    -- first Descriptors from the ItemBase
     iBaseOtherReqs iBase ++ [iBaseLevelReq iBase] ++ iBaseInnates iBase ++ iBaseDescription iBase
        ++
    -- now the binary-specific stuff of Item
    [iLevel] ++ gems ++ innateDefs ++ iEffects ++ iEnchantments ++ iTriggerables
    where
        gems = flip concatMap iGems $ \(_, n, d) -> [n,d]
        innateDefs = map snd iInnateDefs
    
putItem env (Item {..}) = putPartition (encodeLocationBytes env iLocation) iPartition



itemAsBS env item = runPut $ putItem env item




