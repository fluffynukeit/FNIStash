-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.File.Crypto
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

module FNIStash.File.Crypto (
    CryptoFile,
    readCryptoFile,
    writeCryptoFile
) where

-- This module is for reading and writing the scrambled save game files

import qualified Data.ByteString.Lazy as BS
import Data.Tuple.Curry
import Data.Bits (Bits(..))
import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

-- CryptoFile with a b phantom types.  where a is the kind of save file.
data CryptoFile a = CryptoFile {
    fileVersion   :: Word32,
    fileDummy     :: Word8,
    fileChecksum  :: Word32,
    fileGameData  :: BS.ByteString,
    fileSize      :: Word32
    }

getCryptoFile :: Get (CryptoFile a)
getCryptoFile = 
    CryptoFile <$>
    getWord32le <*> -- version
    getWord8 <*> -- dummy byte
    (getWord32le >> return (0x00::Word32)) <*> -- read checksum, throw it away, then use zeros
    (remaining >>= \x -> getLazyByteString (x-4) >>= return . descramble) <*> -- actual file data (descrambled)
    getWord32le -- data size

putCryptoFile :: CryptoFile a -> Put
putCryptoFile desFile = do
    putWord32le (fileVersion desFile)
    putWord8 (fileDummy desFile)
    putWord32le (checksum $ fileGameData desFile)
    putLazyByteString (scramble $ fileGameData desFile)
    putWord32le (4 + 1 + 4 + 4 + fromIntegral (BS.length $ fileGameData desFile))

readCryptoFile filePath =
    BS.readFile filePath >>= return . (runGet getCryptoFile)

writeCryptoFile filePath cryptoFile =
    let fileData = runPut (putCryptoFile cryptoFile)
    in BS.writeFile filePath fileData

-- Low level (de)scrambling functions

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
        else complement constructedByte

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


-- checksum stuff
csSeed = 0x14D3::Word32

checksum :: BS.ByteString -> Word32
checksum bs = BS.foldl'
    (\acc byte -> (shiftL acc 0x5 + acc + fromIntegral byte) .&. 0xFFFFFFFF)
    csSeed bs

