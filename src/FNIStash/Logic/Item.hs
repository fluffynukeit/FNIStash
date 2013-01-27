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
    DamageType (..),
    textItem
) where

import FNIStash.File.General
import FNIStash.File.DAT
import FNIStash.Logic.Translate
import FNIStash.Logic.Variables

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Data.Word
import Data.Int
import Data.Monoid
import Numeric
import Data.Maybe

data Item = Item {
    leadByte :: Word8,
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
    mods :: [[Mod]],
    gems :: [Item]
    }

data Mod = Mod {
    modType :: Word32,
    modName :: T.Text,
    modValueList :: [Float],
    modUnknown1 :: T.Text,
    modEffectIndex :: Word32,
    modDamageType :: DamageType,
    modUnknown2 :: Word32,
    modItemLevel :: Word32,
    modDuration :: Float,
    modUnknown3 :: Word32,
    modValue :: Float,
    modUnknown4 :: Word32
}

data DamageType = Physical | Fire | Electric | Ice | Poison | Unknown
    deriving (Show, Eq)

instance Translate Mod where
    translateMarkup mod markup =
        T.pack $ (case markup of
            "VALUE" -> show . modValue
            "DURATION" -> show . modDuration
            "DMGTYPE" -> show . modDamageType
            "VALUE1" -> show . (flip (!!) 1) . modValueList
            "VALUE2" -> show . (flip (!!) 2) . modValueList
            "VALUE3" -> show . (flip (!!) 3) . modValueList
            "VALUE4" -> show . (flip (!!) 4) . modValueList
            _ -> \mod -> "???"
            ) mod


textItem effSearch i = T.unlines
    ["GUID: " <> (T.pack . show $ (fromIntegral $ guid i::Int64)),
     "Full name: " <> (T.unwords [prefix i, name i, suffix i]),
     "Num Enchants: " <> T.pack (show $ nEnchants i),
     "Item level: " <> T.pack (show $ level i),
     "Used Sockets: " <> T.pack (show $ nSocketsUsed i) <> "/" <> T.pack (show $ nSockets i),
     "Dmg/Armor: " <> T.pack (show $ maxDmg i) <> "/" <> T.pack (show $ armor i),
     "Num elements: " <> T.pack (show $ nElements i),
     "Mods: " ,"", textList (textList (textMod effSearch)) (mods i),
     "Gems: " <> textList (textItem effSearch) (gems i),
     "", ""]

textMod effSearch i =
    let effectNode = effSearch (modEffectIndex i)
        maybeSentence = (effectNode >>= findVar vGOODDES >>= translateVar)
    in case maybeSentence of
        Just sentence -> T.unlines [translateSentence i sentence]
        Nothing -> textModOld i

textModOld i =
    let k f = T.pack . show $ f i
    in T.unlines
        ["","Type: " <> (intToHex . fromIntegral $ modType i),
         "Name: " <> (modName i),
         "Values: " <> (textList (\x -> (T.pack $ show x) <> ", ") $ modValueList i),
         "Unknown1: " <> k modUnknown1,
         "EffectIndex: " <> k modEffectIndex,
         "DmgType: " <> k modDamageType,
         "Unknown2: " <> k modUnknown2 ,
         "ItemLevel: " <> k modItemLevel,
         "Duration: " <> k modDuration,
         "Unknown3: " <> k modUnknown3,
         "Value: " <> k modValue,
         "Unknown4: " <> k modUnknown4,
         ""]
