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

import qualified Data.ByteString as BS
import FNIStash.Logic.File.User.SharedStash
import Data.Binary.Strict.Get (runGet)

ssFileOrig = "C:\\Users\\Dan\\Desktop\\sharedstash_v2.bin"
ssDescrambled = "C:\\Users\\Dan\\Desktop\\sharedstash_haskell.bin"
ssScrambled = "C:\\Users\\Dan\\Desktop\\sharedstash_haskellScrambled.bin"
examineFile = "C:\\Users\\Dan\\Desktop\\shareStashExamine.txt"

main = do
    input <- BS.readFile ssFileOrig
    writeFile examineFile $ inputToString input
    let (Right a, bs) = runGet (getScrambled >>= return . fileGameData . unDescrambled . descrambleGameFile) input
    BS.writeFile ssDescrambled a

inputToString :: BS.ByteString -> String
inputToString input =
    let k = (fst $ runGet getScrambled input) >>=
            return . descrambleGameFile >>=
            bsToItems . fileGameData . unDescrambled
    in either id (\(failItems, succItems) ->
                    unlines ["N Items successfully parsed: " ++ (show $ length succItems),
                            "",
                            concatMap show succItems]) k


