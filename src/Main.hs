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
import FNIStash.Logic.File
import Data.Binary.Strict.Get (runGet)

ssFileOrig = "C:\\Users\\Dan\\Desktop\\sharedstash_v2.bin"
ssDescrambled = "C:\\Users\\Dan\\Desktop\\sharedstash_haskell.bin"
ssScrambled = "C:\\Users\\Dan\\Desktop\\sharedstash_haskellScrambled.bin"
examineFile = "C:\\Users\\Dan\\Desktop\\shareStashExamine.txt"

main = do
    input <- BS.readFile ssFileOrig
    writeFile examineFile $ inputToString input

inputToString :: BS.ByteString -> String
inputToString input = let result = fst $ runGet getScrambled input
                      in case result of
                            Left x -> x
                            Right gfsOrig ->
                                let gfD = descrambleGameFile gfsOrig
                                    f = fileGameData $ unDescrambled gfD
                                    in case bsToItems f of
                                        Left n -> n
                                        Right (failItems, succItems) ->
                                            unlines ["N Items successfully parsed: " ++ (show $ length succItems),
                                                    "",
                                                    concatMap show succItems]




