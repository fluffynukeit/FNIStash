-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.File.Variables
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

module FNIStash.File.Variables where

import FNIStash.File.DAT

import Data.Endian
import Data.Word
import Data.Int
import qualified Data.List.Utils as L

-- This file defines values for different VariableID's that are useful

grab k = lkupVar (swapEndian k::VarID)
search k = searchNodeTree (swapEndian k::VarID)

-- General lookup stuff

newtype ItemGUID = ItemGUID
    { itemGUIDVal ::Int64
    } deriving (Eq, Ord)


vUNIT_GUID d = grab 0x06aad3ed d >>= stringVar >>= return . ItemGUID . read
vNAME d = grab 0xe50d6600 d >>= textVar
vBASEFILE d = grab 0xc52772e2 d >>= textVar
vDISPLAYNAME d = grab 0x767c2f83 d >>= stringVar
vICON d = grab 0xae856500 d >>= stringVar


-- Multiuse failure "lookup"
vUnknown a d = Just a

-- Descriptions of effects

data DescriptionType = GOODDES | GOODDESOT | BADDES | BADDESOT | UnknownDescriptionType deriving (Eq, Ord)
data EffectDescription = EffectDescription
    { effDescType ::DescriptionType
    , effDesc :: String
    } deriving (Eq, Ord)

mkEffDes typ v d = grab v d >>= stringVar >>= return . (EffectDescription typ)

vGOODDES = mkEffDes GOODDES 0xda18d35a
vGOODDESOT = mkEffDes GOODDESOT 0xdfa0624c 
vBADDES = mkEffDes BADDES 0xf2183300
vBADDESOT = mkEffDes BADDESOT 0xb4cf63cc

vDISPLAYPRECISION d = grab 0xcceda5e5 d >>= intVar

-- Lookup stuff for inventory and item lookups
newtype SlotID = SlotID
    { slotIDVal :: Word16
    } deriving (Eq, Ord)
newtype ContainerID = ContainerID
    { containerIDVal :: Word16
    } deriving (Eq, Ord)

vUNIQUEID d = grab 0xdf973b17 d >>= word32Var >>= return . fromIntegral
vSlotID d = vUNIQUEID d >>= return . SlotID . fromIntegral
vContainerID d = vUNIQUEID d >>= return . ContainerID . fromIntegral


-- Item requirements

data StatReq = StatReq Stat Int deriving (Eq, Ord)
data Stat = Strength | Dexterity | Focus | Vitality deriving (Eq, Ord, Show)

check0 0 = Nothing
check0 k = Just k


vSTRENGTH_REQUIRED d = grab 0x6b29dd27 d >>= intVar >>= check0 >>= return . (StatReq Strength)
vDEXTERITY_REQUIRED d = grab 0xea6869a2 d >>= intVar >>= check0 >>= return . (StatReq Dexterity)
vMAGIC_REQUIRED d = grab 0x7bfed9d5 d >>= intVar >>= check0 >>= return . (StatReq Focus)
vDEFENSE_REQUIRED d = grab 0xad0c2391 d >>= intVar >>= check0 >>= return . (StatReq Vitality)

vLEVEL_REQUIRED d = grab 0x791e689d d >>= intVar

vREQ_CLASS d = search 0x0f7e16fa d >>= vUNITTYPE

-- Information about an item

vSPEED d = grab 0xe41c190f d >>= intVar >>= return . (flip (/) 100) . fromIntegral
vMAX_SOCKETS d = grab 0x3fe4a85e d >>= intVar
vRARITY d = grab 0xd82e3820 d >>= intVar
vRANGE d = grab 0xa5b0010f d >>= floatVar

vDESCRIPTION d = grab 0xa2dcb313 d >>= stringVar

-- Damage stuff

data DamageType = Physical | Fire | Electric | Ice | Poison | All | UnknownDamageType
    deriving (Ord, Show, Eq)

data Damage = Damage
    { dType :: DamageType
    , dLow :: Float
    , dHigh :: Float
    } deriving (Eq, Ord)

vMINDAMAGE d = grab 0xfee3360c d >>= intVar
vMAXDAMAGE d = grab 0xbbe3368c d >>= intVar

getDamage varID dType Nothing high d = Nothing
getDamage varID dType low Nothing d = Nothing
getDamage varID dType (Just low) (Just high) d =
    grab varID d >>= intVar >>= check0 >>=
    \x -> return $ Damage dType (fromIntegral x * low/100) (fromIntegral x * high/100)

vDAMAGE_PHYSICAL = getDamage 0x78108146 Physical
vDAMAGE_POISON   = getDamage 0x8fa06905 Poison
vDAMAGE_FIRE     = getDamage 0x1c414a4b Fire
vDAMAGE_ICE      = getDamage 0x5d6f5eca Ice
vDAMAGE_ELECTRIC = getDamage 0xded58f7f Electric

getMod v d = grab v d >>= intVar >>= return . (/100) . fromIntegral :: Maybe Float

vSPEED_DMG_MOD    = getMod 0x525b4287
vRARITY_DMG_MOD   = getMod 0x6d6570a6
vSPECIAL_DMG_MOD  = getMod 0x29bff2ee

-- Unit type stuff

data Quality = NormalQ | MagicQ | UniqueQ | LegendaryQ | NoneQ | UnknownQuality deriving (Eq, Ord)

data UnitType = UnitType
    { uQuality :: Quality
    , uType :: String
    } deriving (Eq, Ord)

vUNITTYPE d = grab 0xfe646b17 d >>= stringVar >>= return . parseType

parseType typeString =
    let usReplaced = L.replace "_" " " typeString
        pieces = words usReplaced
        justOneWord = length pieces == 1
        quality = case pieces !! 0 of
            "NORMAL"    -> NormalQ
            "MAGIC"     -> MagicQ
            "LEGENDARY" -> LegendaryQ
            "UNIQUE"    -> UniqueQ
            _           -> if justOneWord then NoneQ else UnknownQuality
        itemType = if justOneWord then pieces !! 0 else concat $ tail pieces
    in UnitType quality itemType



-- Graphs
vX d = grab 0x78000000 d >>= floatVar
vY d = grab 0x79000000 d >>= floatVar
