-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.File.PAK
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

-- functions for exploring and extracting raw data from PAK files

module FNIStash.File.PAK (
    readPAKMAN,
    pakFiles,
    lkupPAKFile,
    entryData,
    filterMANByPrefix,
    PAKFiles,
    textPAKFiles
) where

import FNIStash.File.General

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.List as L
import Data.Binary.Get

import Data.Word
import qualified Data.Text as T
import Control.Applicative
import Control.Monad

import Codec.Compression.Zlib
import Data.Monoid

import System.IO -- needed for handle functions

----- Worker functions


readPAKMAN fileName = do
    content <- BS.readFile fileName
    return . manHeaderToMAN $ runGet getMANHeader content

filterMANByPrefix :: MAN -> [T.Text] -> MAN
filterMANByPrefix man prefList =
    let matchesPrefix (path,_) = any (flip T.isPrefixOf path) prefList
    in L.filter matchesPrefix man


pakSeek pakFile (file, offset) = do
    withBinaryFile pakFile ReadMode (
        \h -> hSeek h AbsoluteSeek (fromIntegral offset - 4) >>
        return h >>=
        BS.hGetContents >>=
        (\c -> return $! runGet getPAKEntry c) >>=
        return . (,) file
        )
        
pakFiles :: MAN -> FilePath -> IO (PAKFiles)
pakFiles man pakFile =
    mapM (pakSeek pakFile) man >>= return . M.fromList 


lkupPAKFile :: PAKFiles -> FilePath -> Maybe BS.ByteString
lkupPAKFile pakFiles filePath = 
    let entry = flip M.lookup pakFiles $ (T.toUpper . T.pack) filePath
    in fmap entryData entry -- on lookup, decompress the data

entryData = (decompress . pakEncodedData)

fileEntriesOnly entries = L.filter ((Folder /=) . entryType) entries

manHeaderToMAN :: MANHeader -> MAN
manHeaderToMAN hdr =
    let fileName folder entry = (folderName folder <> entryName entry, entryOffset entry)
        folders = headerFolders hdr
        filesInFolder = fileEntriesOnly . folderEntries
    in L.concat $ flip L.map folders (\fol ->
        flip L.map (filesInFolder fol) (fileName fol))

textPAKFiles pakFiles =
        let f (k,entry) = k <> ", " <> ((T.pack . show . BS.length . pakEncodedData) entry) <> "\n"
        in textList f $ M.toList pakFiles


-----  Data Declarations ------

type PAKFiles = M.Map T.Text PAKEntry
type MAN = [(T.Text, Word32)]

data MANEntry = MANEntry {
    entryCrc32 :: Word32,
    entryType :: PAKFileType,
    entryName :: T.Text,
    entryOffset :: Word32,
    entryDecodedSize :: Word32,
    entryUnknown1W32 :: Word32,
    entryUnknown2W32 :: Word32
    } deriving Eq

data MANFolder = MANFolder {
    folderName :: T.Text,
    folderEntries :: [MANEntry]
    } deriving Eq

data MANHeader = MANHeader {
    headerVersion :: Word16,
    headerName :: T.Text,
    headerUnknown1W32 :: Word32,
    headerFolders :: [MANFolder]
    } deriving Eq

data PAKFileType =
    DatTemplate | Layout | Mesh | Skeleton | Dds | Png | OggWav |
    Folder | Material | Raw | Imageset | Ttf | Font | Animation |
    Hie | Scheme | Looknfeel | Mpp | Unrecognized
    deriving (Show, Eq)

data PAKEntry = PAKEntry {
    pakHeader :: Word32,
    pakDecodedSize :: Word32,
    pakEncodedSize :: Word32,
    pakEncodedData :: BS.ByteString
    } deriving Eq

---- Gets -------

getMANEntry :: Get MANEntry
getMANEntry =
    MANEntry <$> getWord32le <*> getFileType <*> getTorchText
             <*> getWord32le <*> getWord32le <*> getWord32le <*> getWord32le

getMANFolder :: Get MANFolder
getMANFolder =
    MANFolder <$> getTorchText
              <*> (getWord32le >>= (flip replicateM getMANEntry) . fromIntegral)

getMANHeader :: Get MANHeader
getMANHeader =
    MANHeader <$> getWord16le
              <*> getTorchText
              <*> getWord32le
              <*> (getWord32le >>= (flip replicateM getMANFolder) . fromIntegral)
 
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
        0x06 -> OggWav
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

getPAKEntry :: Get PAKEntry
getPAKEntry = do
    hdr <- getWord32le
    decSize <- getWord32le
    encSize <- getWord32le
    encData <- getLazyByteString (fromIntegral encSize) >>= return . BS.copy
    return $ PAKEntry hdr decSize encSize $! encData









