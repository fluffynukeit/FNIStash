-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.Logic.Items
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

{-# LANGUAGE RecordWildCards #-}

module FNIStash.Logic.Operations (
    saveItems
) where

import FNIStash.File.SharedStash
import FNIStash.File.Crypto
import Data.List
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS


-- START ITEM OPERATIONS

sharedStashToBS env ss = runPut (putSharedStash env ss)

saveItems env c ss filePath = do
    let i = sharedStashToBS env ss
        newSaveFile = CryptoFile (fileVersion c) (fileDummy c) (0) (i) (0)
    writeCryptoFile filePath newSaveFile
    return (ss, [])
