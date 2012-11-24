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
    formGameFile
) where

import qualified Data.ByteString.Lazy as BS
import FNIStash.Logic.Crypto
import Data.Binary.Get (getWord32be, runGet)
import Data.Binary (encode)
import Data.Word (Word32)


data GameFile = GameFile {
    fileVersion     :: BS.ByteString,
    fileDummy       :: BS.ByteString,
    fileChecksum    :: BS.ByteString,
    fileGameData    :: BS.ByteString,
    fileFooter      :: BS.ByteString
    }
newtype Scrambled = Scrambled GameFile
newtype Descrambled = Descrambled GameFile

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
    (footer   $ fileGameData desFile)

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
footer gameData = BS.reverse . encode $
    ((fromIntegral $ vl + csl + dl + fl + BS.length gameData)::Word32)
