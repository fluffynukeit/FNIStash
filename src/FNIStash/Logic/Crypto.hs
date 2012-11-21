-----------------------------------------------------------------------------
--
-- Module      :  FNI.Logic.Crypto
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module FNIStash.Logic.Crypto (
    descramble
) where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Tuple.Curry
import Data.Bits (Bits(..))

-- modeled after Jonathan Gevaryahu AKA Lord Nightmare's descramble c code
descramble dataString = let
    bytePairs = BS.zip dataString $ BS.reverse dataString
    in BS.pack $ map (uncurryN desByteMerger) bytePairs


desByteMerger fByte rByte = let
    leastSigNyb = flip shiftR 4 $ fByte .&. 0xF0
    mostSigNyb = rByte .&. 0x0F
    in if (mostSigNyb == 0 && leastSigNyb == 0) || (mostSigNyb == 0xF && leastSigNyb == 0xF)
        then flip shiftL 4 mostSigNyb .|. leastSigNyb
        else flip shiftL 4 (mostSigNyb `xor` 0xF) .|. (leastSigNyb `xor` 0xF)


