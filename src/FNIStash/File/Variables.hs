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


-- This file defines values for different VariableID's that are useful

grab k = lkupVar (swapEndian k::VarID)

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

-- Information about an item

data ItemClass = Axe2h | Mace2h | Sword2h | Axe1h | Bow | Cannon | Crossbow | Fist | Mace1h
               | Pistol | Polearm | Rifle | Shield | Staff | Sword1h | Wand | Necklace | Belt
               | Boots | ChestArmor | Collar | Gloves | Helmet | Pants | Ring | ShoulderArmor
               | Stud | Socketable | Axe | Sword | UnknownItemClass deriving (Eq, Ord)

r a = [(a, "")]
instance Read ItemClass where
    readsPrec _ "2HAXE"     = r Axe2h
    readsPrec _ "2HMACE"    = r Mace2h
    readsPrec _ "2HSWORD"   = r Sword2h
    readsPrec _ "1HAXE"     = r Axe1h
    readsPrec _ "BOW"       = r Bow
    readsPrec _ "CANNON"    = r Cannon
    readsPrec _ "CROSSBOW"  = r Crossbow
    readsPrec _ "FIST"      = r Fist
    readsPrec _ "1HMACE"    = r Mace1h
    readsPrec _ "PISTOL"    = r Pistol
    readsPrec _ "POLEARM"   = r Polearm
    readsPrec _ "AXE"       = r Axe
    readsPrec _ "SWORD"     = r Sword
    readsPrec _ "RIFLE"     = r Rifle
    readsPrec _ "SHIELD"    = r Shield
    readsPrec _ "STAFF"     = r Staff
    readsPrec _ "1HSWORD"   = r Sword1h
    readsPrec _ "HELMET"    = r Helmet
    readsPrec _ "NECKLACE"  = r Necklace
    readsPrec _ "BELT"      = r Belt
    readsPrec _ "BOOTS"     = r Boots
    readsPrec _ "CHEST ARMOR" = r ChestArmor
    readsPrec _ "COLLAR"    = r Collar
    readsPrec _ "GLOVES"    = r Gloves
    readsPrec _ "PANTS"     = r Pants
    readsPrec _ "RING"      = r Ring
    readsPrec _ "SHOULDER ARMOR" = r ShoulderArmor
    readsPrec _ "STUD"      = r Stud
    readsPrec _ "WAND"      = r Wand
    readsPrec _ _           = r UnknownItemClass


data Quality = NormalQ | MagicQ | UniqueQ | UnknownQuality deriving (Eq, Ord)


data StatReq = Strength Int | Dexterity Int | Magic Int | Vitality Int deriving (Eq, Ord, Show)

check0 0 = Nothing
check0 k = Just k

vSTRENGTH_REQUIRED d = grab 0x6b29dd27 d >>= intVar >>= check0 >>= return . Strength
vDEXTERITY_REQUIRED d = grab 0xea6869a2 d >>= intVar >>= check0 >>= return . Dexterity
vMAGIC_REQUIRED d = grab 0x7bfed9d5 d >>= intVar >>= check0 >>= return . Magic
vDEFENSE_REQUIRED d = grab 0xad0c2391 d >>= intVar >>= check0 >>= return . Vitality

vSPEED d = grab 0xe41c190f d >>= intVar
vMAX_SOCKETS d = grab 0x3fe4a85e d >>= intVar
vRARITY d = grab 0xd82e3820 d >>= intVar
vRANGE d = grab 0xa5b0010f d >>= floatVar
