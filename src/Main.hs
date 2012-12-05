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
import FNIStash.Logic.File
import Data.Binary.Get (runGet)

ssFileOrig = "C:\\Users\\Dan\\Desktop\\sharedstash_v2.bin"
ssDescrambled = "C:\\Users\\Dan\\Desktop\\sharedstash_haskell.bin"
ssScrambled = "C:\\Users\\Dan\\Desktop\\sharedstash_haskellScrambled.bin"
examineFile = "C:\\Users\\Dan\\Desktop\\shareStashExamine.txt"

main = do
    input <- BS.readFile ssFileOrig
    let gfSOrig = parseGameFile input
    let gfD = descrambleGameFile gfSOrig
    let gfS = scrambleGameFile gfD
    BS.writeFile ssScrambled $ formGameFile gfS
    BS.writeFile ssDescrambled $ liftD fileGameData gfD
    writeFile examineFile $ show $ runGet dataGetItems $ liftD fileGameData gfD






