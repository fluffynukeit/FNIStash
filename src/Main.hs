{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Tuple.Curry
import Numeric
import Data.Bits (Bits(..))

ssFile = "C:\\Users\\Dan\\Desktop\\sharedstash_v2.bin"
ssDFile = "C:\\Users\\Dan\\Desktop\\sharedstash_haskell.bin"

main = do
    input <- BS.readFile ssFile
    let sections = parseFileSections input
    let procSections = processFileSections sections
    BS.writeFile ssDFile $ combineFileSections procSections
    print "Finished"

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
    in (versionString, dummyString, checkSumString, dataString, footerString)

combineFileSections (vs, dus, cs, ds, fs) = BS.concat [vs, dus, cs, ds, fs]

processFileSections (vs, dus, cs, ds, fs) = (vs, dus, BS.replicate csl 0x00, descramble ds, fs)

streamToHex :: BS.ByteString -> String
streamToHex = ("0x" ++) . concatMap ((" "++) . showHexPadded) . BS.unpack

showHexPadded word = case length $ showHex word "" of
    1 -> "0" ++ showHex word ""
    2 -> showHex word ""

descramble dataString = let
    bytePairs = BS.zip dataString $ BS.reverse dataString
    in BS.pack $ map (uncurryN byteMerger) bytePairs


byteMerger fByte rByte = let
    leastSigNyb = flip shiftR 4 $ fByte .&. 0xF0
    mostSigNyb = rByte .&. 0x0F
    in if (mostSigNyb == 0 && leastSigNyb == 0) || (mostSigNyb == 0xF && leastSigNyb == 0xF)
        then flip shiftL 4 mostSigNyb .|. leastSigNyb
        else flip shiftL 4 (mostSigNyb `xor` 0xF) .|. (leastSigNyb `xor` 0xF)


