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

module FNIStash.File.PAK (
    readPAKMAN
) where


import FNIStash.File.General

import Data.ByteString as BS hiding (length)
import Data.Binary.Strict.Get
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy
import Data.Word
import Data.Text hiding (length)
import Control.Applicative
import Control.Monad (replicateM)
import GHC.TopHandler (runIO)


type PAKHierarchy = Map


readPAKMAN fileName = do
    content <- BS.readFile fileName
    return $ runGet getMANHeader content



data MANEntry = MANEntry {
    entryCrc32 :: Word32,
    entryType :: PAKFileType,
    entryName :: Text,
    entryOffset :: Word32,
    entryDecodedSize :: Word32,
    entryUnknown1W32 :: Word32,
    entryUnknown2W32 :: Word32
    }

instance Show MANEntry where
    show me = (show . entryType) me ++ " named " ++ (show . entryName) me ++ "\n"

getMANEntry :: Get MANEntry
getMANEntry =
    MANEntry <$> getWord32le <*> getFileType <*> getTorchText
             <*> getWord32le <*> getWord32le <*> getWord32le <*> getWord32le




data MANFolder = MANFolder {
    folderName :: Text,
    folderEntries :: [MANEntry]
    }

instance Show MANFolder where
    show mf = "-- Contents folder " ++ (show . folderName) mf ++
              "(" ++ (show . length . folderEntries) mf ++ " entries)\n" ++
              (show . folderEntries) mf

getMANFolder :: Get MANFolder
getMANFolder =
    MANFolder <$> getTorchText
              <*> (getWord32le >>= (flip replicateM getMANEntry) . fromIntegral)





data MANHeader = MANHeader {
    headerVersion :: Word16,
    headerName :: Text,
    headerUnknown1W32 :: Word32,
    headerFolders :: [MANFolder]
    }

instance Show MANHeader where
    show mh = "Hierarchy for " ++ (show . headerName) mh ++ "\n" ++
              (show . headerFolders) mh

getMANHeader :: Get MANHeader
getMANHeader =
    MANHeader <$> getWord16le
              <*> getTorchText
              <*> getWord32le
              <*> (getWord32le >>= (flip replicateM getMANFolder) . fromIntegral)



data PAKFileType =
    DatTemplate | Layout | Mesh | Skeleton | Dds | Png | OggWav |
    Folder | Material | Raw | Imageset | Ttf | Font | Animation |
    Hie | Scheme | Looknfeel | Mpp | Unrecognized
    deriving Show


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
        _    -> Unrecognized



