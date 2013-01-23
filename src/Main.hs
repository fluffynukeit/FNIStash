-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  2013 Daniel Austin
-- License     :  AllRightsReserved
--
-- Maintainer  :  dan@fluffynukeit.com
-- Stability   :  Development
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import FNIStash.File.PAK
import FNIStash.File.DAT
import FNIStash.Logic.Search
import FNIStash.File.SharedStash
import FNIStash.File.General
import FNIStash.File.Crypto

import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BS
import Data.Maybe

testDir = "C:\\Users\\Dan\\Desktop\\FNI Testing"
pakMANFileBinary = "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Torchlight II\\PAKS\\DATA.PAK.MAN"
pakFileBinary = "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Torchlight II\\PAKS\\DATA.PAK"
sharedStashBinary = testDir </> "sharedstash_haskell.bin"
sharedStashCrypted = testDir </> "sharedstash_v2.bin"
sharedStashTxt = testDir </> "sharedStashTxt.txt"

fromRight (Right a) = a
    
main = do
    man <- readPAKMAN pakMANFileBinary
    let pak = pakFiles man pakFileBinary
    findItem <- itemSearcher pak
    T.writeFile (testDir </> "testOutputDAT.txt") $ (textDAT . fromJust . findItem) "-1053906477868677616"
    s <- readCryptoFile sharedStashCrypted >>= (return . fileGameData . fromRight) >>= BS.writeFile sharedStashBinary
    ssData <- BS.readFile sharedStashBinary
    let sharedStashResult = runGetWithFail "Problem reading shared stash" getSharedStash ssData
    T.writeFile sharedStashTxt $
        case sharedStashResult of
            Left error -> error
            Right sharedStash -> (textSharedStash sharedStash)


