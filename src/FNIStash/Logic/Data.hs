-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.Logic.Data
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

module FNIStash.Logic.Data (
    Item(..),
    Mod(..),
    streamToHex
) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Numeric
import Data.Word (Word16, Word32)

data Item = Item {
    model :: BS.ByteString,
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
    mods :: Either String [Either String Mod]
    }

data Mod = Mod {
    modType :: Word32,
    modName :: T.Text,
    modData :: BS.ByteString
}

instance Show Item where
    show = itemShow

itemShow i = unlines
    ["Full name: " ++ (T.unpack $ T.unwords [prefix i, name i, suffix i]),
     "Num Enchants: " ++ (show $ nEnchants i),
     "Item level: " ++ (show $ level i),
     "Used Sockets: " ++ (show $ nSocketsUsed i) ++ "/" ++ (show $ nSockets i),
     "Dmg/Armor: " ++ (show $ maxDmg i) ++ "/" ++ (show $ armor i),
     "Num elements: " ++ (show $ nElements i),
     "Num mods: " ++ (either (\x -> "Unknown: " ++ x) (show . length) $ mods i),
     "Mods: " ++ (show $ mods i),
     "", ""]

instance Show Mod where
    show = modShow

modShow i = unlines
    ["","","Type: " ++ (show $ modType i),
     "Name: " ++ (show . T.unpack . modName $ i),
     "Data: " ++ (streamToHex $ modData i),
     "",""]

streamToHex :: BS.ByteString -> String
streamToHex = ("0x" ++) . concatMap ((" "++) . showHexPadded) . BS.unpack

showHexPadded word = case length $ showHex word "" of
    1 -> "0" ++ showHex word ""
    2 -> showHex word ""


