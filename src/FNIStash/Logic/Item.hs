-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.Logic.Item
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


module FNIStash.Logic.Item (
    Item (Item),
    Mod (Mod),
    textItem
) where

import FNIStash.File.General

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Data.Word
import Data.Int
import Data.Monoid

data Item = Item {
    guid :: Word64,
    name :: T.Text,
    prefix :: T.Text,
    suffix :: T.Text,
    serial :: BS.ByteString,
    bytes1 :: BS.ByteString, -- 00 FFx24 00 4x
    nEnchants :: Word32,
    location :: Word16,
    bytes2 :: BS.ByteString, -- 18 00 00 01 01 01 01 00 01
    bytes3 :: BS.ByteString,
    bytes4 :: [BS.ByteString],
    level :: Word32,
    bytes5 :: BS.ByteString, -- 01 00 00 00 always?
    nSockets :: Word32,
    nSocketsUsed :: Word32,
    bytes6 :: BS.ByteString,
    maxDmg :: Word32,
    armor :: Word32,
    bytes7 :: BS.ByteString,
    bytes8 :: BS.ByteString, -- FFx12
    nElements :: Word16,
    elements :: [BS.ByteString],
    mods :: [[Mod]]
    }

data Mod = Mod {
    modType :: Word32,
    modName :: T.Text,
    modData :: BS.ByteString
}

textItem i = T.unlines
    ["GUID: " <> (T.pack . show $ (fromIntegral $ guid i::Int64)),
     "Full name: " <> (T.unwords [prefix i, name i, suffix i]),
     "Num Enchants: " <> T.pack (show $ nEnchants i),
     "Item level: " <> T.pack (show $ level i),
     "Used Sockets: " <> T.pack (show $ nSocketsUsed i) <> "/" <> T.pack (show $ nSockets i),
     "Dmg/Armor: " <> T.pack (show $ maxDmg i) <> "/" <> T.pack (show $ armor i),
     "Num elements: " <> T.pack (show $ nElements i),
     "Mods: " <> T.pack (show $ mods i),
     "", ""]

instance Show Mod where
    show i = unlines
        ["","","Type: " ++ (show $ modType i),
         "Name: " ++ T.unpack (modName i),
         "Data: " ++ (T.unpack . streamToHex $ modData i),
         "",""]

