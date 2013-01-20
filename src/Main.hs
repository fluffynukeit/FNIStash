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
import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe

testDir = "C:\\Users\\Dan\\Desktop\\FNI Testing"
pakMANFileBinary = "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Torchlight II\\PAKS\\DATA.PAK.MAN"
pakFileBinary = "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Torchlight II\\PAKS\\DATA.PAK"
    
main = do
    man <- readPAKMAN pakMANFileBinary
    let pak = pakFiles man pakFileBinary
    findItem <- itemSearcher pak
    T.writeFile (testDir </> "testOutputDAT.txt") $ (textDAT . fromJust . findItem) "-1053906477868677616"


