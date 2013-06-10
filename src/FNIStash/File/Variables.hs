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

data DescriptionType = GOODDES | GOODDESOT | BADDES | BADDESOT | UnknownDescriptionType
data EffectDescription = EffectDescription
    { effDescType ::DescriptionType
    , effDesc :: String
    }

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
