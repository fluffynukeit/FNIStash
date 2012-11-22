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

module FNIStash.Logic.File

where

import qualified Data.ByteString.Lazy as BS
import FNIStash.Logic.Crypto
import Data.Binary.Get (getWord32be, runGet)

csl = 4 -- checksum length (bytes)
fl = 4  -- footer length (bytes)

parseFileSections fileBS = let
    (versionString, afterVers) = BS.splitAt fl fileBS
    version = toInteger $ runGet getWord32be versionString
    (dummyString, afterDummy) = BS.splitAt 1 afterVers
    (checkSumString, afterCheckSum) = case version of
        0x40 -> BS.splitAt 0 $ afterDummy   -- version 0x40 has no checksum
        0x41 -> BS.splitAt csl $ afterDummy
        0x42 -> BS.splitAt csl $ afterDummy
        _    -> BS.splitAt csl $ afterDummy
    (dataString, footerString) = BS.splitAt (BS.length afterCheckSum - fl) afterCheckSum
    in [versionString, dummyString, checkSumString, dataString, footerString]

combineFileSections = BS.concat

processFileSections (vs:dus:cs:ds:fs) = [vs, dus, BS.replicate csl 0x00, descramble ds] ++ fs
