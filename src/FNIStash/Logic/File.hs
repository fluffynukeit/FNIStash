-----------------------------------------------------------------------------
--
-- Module      :  FNI.Logic.File
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

module FNIStash.Logic.File (
    parseGameFile,
    descrambleGameFile,
    scrambleGameFile,
    formGameFile,
    dataGetItems,
    fileGameData,
    liftD
) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C
import FNIStash.Logic.Crypto
import FNIStash.Logic.Data (Item(..), streamToHex)
import Data.Binary.Get
       (bytesRead, getWord32le, getLazyByteString, getWord16le,
        getWord32be, runGet)
import Data.Binary (Get, Binary, encode, get)
import Data.Word (Word64, Word32)
import Control.Monad (replicateM, ap, liftM)
import Data.Int (Int64)
import Debug.Trace (trace)
import Control.Applicative ((<$>))


data GameFile = GameFile {
    fileVersion     :: BS.ByteString,
    fileDummy       :: BS.ByteString,
    fileChecksum    :: BS.ByteString,
    fileGameData    :: BS.ByteString,
    fileFooter      :: BS.ByteString
    }
newtype Scrambled = Scrambled GameFile
newtype Descrambled = Descrambled GameFile

liftD f (Descrambled gf) = f gf

-- NOTE: maybe I should write GameFile as an instance of Binary to have encode/decode functions

parseGameFile fileBS = let
    (versionString, afterVers) = BS.splitAt vl fileBS
    version = toInteger $ runGet getWord32be versionString
    (dummyString, afterDummy) = BS.splitAt dl afterVers
    (checksumString, afterChecksum) = case version of
        0x40 -> BS.splitAt 0 $ afterDummy   -- version 0x40 has no checksum
        0x41 -> BS.splitAt csl $ afterDummy
        0x42 -> BS.splitAt csl $ afterDummy
        _    -> BS.splitAt csl $ afterDummy -- assume same structure for future file versions?
    (dataString, footerString) = BS.splitAt (BS.length afterChecksum - fl) afterChecksum
    in Scrambled $ GameFile versionString dummyString checksumString dataString footerString


descrambleGameFile :: Scrambled -> Descrambled
descrambleGameFile (Scrambled scrFile) = Descrambled $ GameFile
    (fileVersion scrFile)
    (fileDummy scrFile)
    (BS.replicate csl 0x00)
    (descramble $ fileGameData scrFile)
    (fileFooter scrFile)

scrambleGameFile :: Descrambled -> Scrambled
scrambleGameFile (Descrambled desFile) = Scrambled $ GameFile
    (fileVersion desFile)
    (fileDummy desFile)
    (checksum $ fileGameData desFile)
    (scramble $ fileGameData desFile)
    (makeFooter   $ fileGameData desFile)

formGameFile (Scrambled gameFile) = BS.concat
    [fileVersion gameFile,
     fileDummy gameFile,
     fileChecksum gameFile,
     fileGameData gameFile,
     fileFooter gameFile]

vl = 4  -- version length (bytes)
csl = 4 -- checksum length (bytes)
fl = 4  -- footer length (bytes)
dl = 1  -- dummy byte length

-- footer
makeFooter gameData = BS.reverse . encode $
    ((fromIntegral $ vl + csl + dl + fl + BS.length gameData)::Word32)


getTorchString :: Get BS.ByteString
getTorchString = fromIntegral . (*2) <$> getWord16le >>= getLazyByteString

getMod = BS.concat <$> sequence [getLazyByteString 4, getTorchString, getLazyByteString 43]

getNextNonzero :: Get Word32
getNextNonzero = do x <- getWord32le
                    if x /= 0 then (return x) else getNextNonzero


u = C.unpack

-- :p k

dataGet :: Get Item
dataGet = do itemLength <- getWord16le
             r1 <- bytesRead
             trace (show itemLength) return ()
             model <- getLazyByteString 11
             name <- getTorchString
             prefix <- getTorchString
             suffix <- getTorchString
             serial <- getLazyByteString 24
             bytes1 <- getLazyByteString 29
             nEnchants <- fromIntegral <$> getWord32le
             location <- fromIntegral <$> getWord16le
             bytes2 <- getLazyByteString 9
             bytes3 <- getLazyByteString 8
             bytes4 <- sequence $ replicate 4 $ getLazyByteString 20
             level <- fromIntegral <$> getWord32le
             bytes5 <- getLazyByteString 4
             nSockets <- fromIntegral <$> getWord32le
             nUsedSockets <- fromIntegral <$> getWord32le
             bytes6 <- getLazyByteString 4
             maxDmg <- fromIntegral <$> getWord32le
             armor <- fromIntegral <$> getWord32le
             bytes7 <- getLazyByteString 4
             bytes8 <- getLazyByteString 12
             nElements <- fromIntegral <$> getWord16le
             elements <- sequence $ replicate (fromIntegral nElements) $ getLazyByteString 12
             trace (streamToHex bytes8 ++ ", " ++ show nElements) return ()
             nMods <- fromIntegral <$> getWord32le
             mods <- sequence $ replicate (fromIntegral nMods) $ getMod
             r2 <- bytesRead
             footer <- getLazyByteString (fromIntegral itemLength - (r2 - r1) + 2)
             trace (streamToHex footer) return ()
             return (Item model (u name) (u prefix) (u suffix) serial bytes1 nEnchants location bytes2 bytes3
                        bytes4 level bytes5 nSockets nUsedSockets bytes6 maxDmg armor bytes7 bytes8
                        nElements elements nMods mods footer)

dataGetItems = fromIntegral <$> getWord32le >>=
                \x -> replicateM 4 dataGet
