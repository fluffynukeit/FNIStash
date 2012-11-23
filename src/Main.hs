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

import Numeric
import qualified Data.ByteString.Lazy as BS
import FNIStash.Logic.File
import FNIStash.Logic.Crypto

ssFile = "C:\\Users\\Dan\\Desktop\\sharedstash_v2.bin"
ssDFile = "C:\\Users\\Dan\\Desktop\\sharedstash_haskell.bin"
ssDScrambled = "C:\\Users\\Dan\\Desktop\\sharedstash_haskellScrambled.bin"

main = do
    input <- BS.readFile ssFile
    let sections = parseFileSections input
    let procSections@(v:dum:c:d:f) = processFileSections sections
    BS.writeFile ssDFile $ combineFileSections procSections
    let calcCS = checksum d
    let calcF = footer d
    BS.writeFile ssDScrambled $ combineFileSections $ [v, dum, calcCS, scramble d] ++ f
    print $ "Checksum, footer are " ++ streamToHex calcCS ++ ", " ++ streamToHex calcF


streamToHex :: BS.ByteString -> String
streamToHex = ("0x" ++) . concatMap ((" "++) . showHexPadded) . BS.unpack

showHexPadded word = case length $ showHex word "" of
    1 -> "0" ++ showHex word ""
    2 -> showHex word ""



