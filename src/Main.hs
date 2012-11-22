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

ssFile = "C:\\Users\\Dan\\Desktop\\sharedstash_v2.bin"
ssDFile = "C:\\Users\\Dan\\Desktop\\sharedstash_haskell.bin"

main = do
    input <- BS.readFile ssFile
    let sections = parseFileSections input
    let procSections = processFileSections sections
    BS.writeFile ssDFile $ combineFileSections procSections
    print "Finished"


streamToHex :: BS.ByteString -> String
streamToHex = ("0x" ++) . concatMap ((" "++) . showHexPadded) . BS.unpack

showHexPadded word = case length $ showHex word "" of
    1 -> "0" ++ showHex word ""
    2 -> showHex word ""



