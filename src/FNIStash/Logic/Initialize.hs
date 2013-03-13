-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.Logic.Initialize
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

module FNIStash.Logic.Initialize (

    stashText

) where

import FNIStash.Logic.Config
import FNIStash.Logic.Env
import FNIStash.File.SharedStash
import FNIStash.File.General
import FNIStash.File.Crypto
import FNIStash.File.PAK
import Filesystem
import Filesystem.Path
import Filesystem.Path.CurrentOS

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Binary.Strict.Get
import Data.Maybe
import Data.Configurator
import Control.Monad.Reader
import Data.Monoid
import Prelude hiding (readFile, writeFile)
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types
import Text.XML.Light
import qualified Codec.Image.DevIL as I
import Data.Array.Unboxed
import Data.Array.IArray
import GHC.Exts
import Data.Word
import Debug.Trace

testDir = "C:\\Users\\Dan\\Desktop\\FNI Testing"
sharedStashCrypted = testDir </> "sharedstash_v2.bin"

envPrefixes = ["MEDIA/EFFECTSLIST.DAT", "MEDIA/UNITS/ITEMS", "MEDIA/SKILLS"]

stashText = do
    appRoot <- ensureAppRoot
    cfg <- ensureConfig appRoot
    ensureGUIAssets appRoot cfg
    pak <- readPAKPrefixes cfg envPrefixes
    let env = buildEnv pak
    ssData <- readCryptoFile (encodeString sharedStashCrypted) >>= return . fileGameData
    let sharedStashResult = runGetWithFail "Can't read shared stash file!" (getSharedStash env) (toStrict ssData)
    return $ case sharedStashResult of
        Left error -> error
        Right sharedStash -> (runReader (textSharedStash sharedStash) env)

readPAKPrefixes cfg prefs = do
    pakMANFileBinary <- require cfg "MANFILE"
    man <- readPAKMAN pakMANFileBinary
    let subMan = filterMANByPrefix man prefs
    pakFileBinary <- require cfg "PAKFILE"
    pakFiles subMan pakFileBinary

ensureAppRoot = do
    -- create the program directory if it doesn't exist
    appRoot <- getAppDataDirectory "FNIStash"
    createTree appRoot
    return appRoot

ensureConfig appRoot = do
    docPath <- getDocumentsDirectory
    -- write out a config file if one does not exist
    let confPath = appRoot </> "Backend.conf"
    confExists <- isFile confPath
    if confExists then
        return ()
        else
            writeConfigOut confPath $ defaultConfigOut docPath
    load [Required (encodeString confPath)]

guiAssets = fmap ("MEDIA/UI/ICONS/" <>) ["ARMOR", "FISH", "GEMS", "JEWELRY", "MISC",
                                         "POTIONS", "QUESTITEMS", "WEAPONS"]

ensureGUIAssets appRoot cfg = do
    let assetPath = appRoot </> "GUIAssets"
    assetsExist <- isDirectory assetPath
    when (not assetsExist) $ writeAssets assetPath cfg

writeAssets assetPath cfg = do
    createTree assetPath
    guiPAK <- readPAKPrefixes cfg guiAssets
    let imageSets = map entryData (M.elems $ pakKeysContaining ".IMAGESET" guiPAK)
    mapM_ (processImageSet assetPath guiPAK) imageSets

processImageSet assetPath pak iset = do
    let rootElement = flip (!!) 1 $ onlyElems $ parseXML iset
        Just imageFilePath = fmap (T.toUpper . T.pack) $ findAttr "Imagefile" rootElement
        tempFileName = assetPath </> (filename $ fromText imageFilePath)
        Just imageFile = lkupPAKFile imageFilePath pak
    writeFile tempFileName imageFile
    I.ilInit
    dds <- I.readImage $ encodeString tempFileName
    let subImagesXML = elChildren rootElement
        subImagesData = map subImageData subImagesXML
    mapM (writeSubImage dds assetPath) subImagesData
    fileExists <- isFile tempFileName
    (when fileExists $ removeFile tempFileName) :: IO ()


writeSubImage dds assetPath (n,x,y,w,h) = do
    let newImg = ulSubImage x y w h dds
    I.writeImage (encodeString (assetPath </> (fromText n) <.> "png")) newImg

subImageData el =
    let k = ( T.pack . fromJust $ findAttr "Name" el, read . fromJust $ findAttr "XPos" el
            , read . fromJust $ findAttr "YPos" el, read . fromJust $ findAttr "Width" el
            , read . fromJust $ findAttr "Height" el)
    in k


-- In XML imageset files, icons are described with origin in top left.  In DevIL library
-- images have origin at bottom left, so need to translate and flip y axis
ulSubImage x y w h imageArray =
    let ((minX, minY, minC), (maxX, maxY, maxC)) = bounds imageArray
        imHeight = maxY - minY
        y' = imHeight - y - h + 1
    in subImage x y' w h imageArray

-- General subImage for DevIL library
subImage:: Int -> Int -> Int -> Int -> UArray (Int, Int, Int) Word8 -> UArray (Int, Int, Int) Word8
subImage x y w h imageArray =
    let indValues = assocs imageArray
        inWindow ((r,c,ch), val) = r >= y && r < y+h && c >= x && c < x+w
        translate ((r,c,ch), val) = ((r-y, c-x, ch), val)
        newArray = array ((0,0,0), (h-1,w-1,3)) $ map translate $ (filter inWindow) indValues
    in newArray


instance IsString QName where
    fromString = unqual

