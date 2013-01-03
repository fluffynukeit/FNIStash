-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.Logic.File.App.PAK
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



-- functions for exploring and extracting raw data from PAK files

module FNIStash.Logic.File.App.PAK (

) where


import Codec.Compression.Zlib (decompress)
import Data.ByteString as BS
import Data.Binary.Get
import qualified Data.Map.Lazy as Map
import qualified Data.Text

type PAKHierarchy = Map

data PAKFileType =
    DatTemplate | Layout | Mesh | Skeleton | Dds | Png | OggWav |
    Folder | Material | Raw | Imageset | Ttf | Font | Animation |
    Hie | Scheme | Looknfeel | Mpp | Unrecognized


data MANEntry = MANEntry {
    crc32 :: Word32,
    fileType :: PAKFileType,
    name :: Text,
    pakOffset :: Word32,
    decodedSize :: Word32,
    unknown1W32 :: Word32,
    unknown2W32 :: Word32
    }



getMANEntry :: Get MANEntry
getMANEntry = do
    crc <- getWord32le
    fType <- getFileType



getFileType :: Get PAKFileType
getFileType = do
    typeID <- getWord8
    return $ case typeID of
        0x00 -> DatTemplate
        0x01 -> Layout
        0x02 -> Mesh
        0x03 -> Skeleton
        0x04 -> Dds
        0x05 -> Png
        0x06 -> Dds
        0x07 -> Folder
        0x08 -> Material
        0x09 -> Raw
        0x0B -> Imageset
        0x0C -> Ttf
        0x0D -> Font
        0x10 -> Animation
        0x11 -> Hie
        0x13 -> Scheme
        0x14 -> Looknfeel
        0x15 -> Mpp



