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
    descramble,
    scramble
) where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Tuple.Curry
import Data.Bits (Bits(..))


descramble = processScrambler desByteMerger
scramble = processScrambler scrByteMerger


-- modeled after Jonathan Gevaryahu AKA Lord Nightmare's scramlbe/descramble c code, then
-- cleaned up a bit so the logic is more obvious to me
processScrambler merger dataString =  let
    bytePairs = BS.zip dataString $ BS.reverse dataString
    in BS.pack $ map (uncurryN merger) bytePairs

desByteMerger forByte revByte = let
    forLeft = leftNyb forByte
    revRight = rightNyb revByte
    constructedByte = mergeNybs revRight forLeft
    in if (constructedByte == 0 || constructedByte == 0xFF)
        then constructedByte
        else constructedByte `xor` 0xFF

scrByteMerger forByte revByte = let
    forRight = rightNyb forByte
    revLeft = leftNyb revByte
    revertNyb byte n = if (byte == 0 || byte == 0xFF) then n else invertNyb n
    in mergeNybs (revertNyb forByte forRight) (revertNyb revByte revLeft)


-- helper functions
leftNyb byte = flip shiftR 4 $ byte .&. 0xF0
rightNyb = (.&. 0x0F)
mergeNybs left right = (flip shiftL 4 left) .|. right
invertNyb n = (.&.) 0x0F $ xor n 0x0F
