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

import FNIStash.File.VarIDs (VarID)
import Data.Endian

-- Variables that are particularly useful.
vUNIT_GUID = swapEndian 0x06aad3ed::VarID
vEFFECTLIST = swapEndian 0x15ca47c3::VarID
vEFFECT = swapEndian 0x351c420e::VarID
vGOODDES = swapEndian 0xda18d35a::VarID
vNAME = swapEndian 0xe50d6600::VarID

