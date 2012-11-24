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

ssFileOrig = "C:\\Users\\Dan\\Desktop\\sharedstash_v2.bin"
ssDescrambled = "C:\\Users\\Dan\\Desktop\\sharedstash_haskell.bin"
ssScrambled = "C:\\Users\\Dan\\Desktop\\sharedstash_haskellScrambled.bin"

main = do
    input <- BS.readFile ssFileOrig
    let gfSOrig = parseGameFile input
    let gfD = descrambleGameFile gfSOrig
    let gfS = scrambleGameFile gfD
    BS.writeFile ssScrambled $ formGameFile gfS



streamToHex :: BS.ByteString -> String
streamToHex = ("0x" ++) . concatMap ((" "++) . showHexPadded) . BS.unpack

showHexPadded word = case length $ showHex word "" of
    1 -> "0" ++ showHex word ""
    2 -> showHex word ""



