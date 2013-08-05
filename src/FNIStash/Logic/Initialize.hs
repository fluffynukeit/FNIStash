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
    initialize,
    ensureAppRoot,
    ensureHtml
) where

-- This file contains stuff for initialization before normal processing.


-- FNIStash stuff
import FNIStash.Logic.Config
import FNIStash.Logic.Env
import FNIStash.Logic.DB
import FNIStash.File.SharedStash
import FNIStash.File.Crypto
import FNIStash.File.PAK
import FNIStash.Comm.Messages

-- Filesystem stuff
import Filesystem
import Filesystem.Path
import Filesystem.Path.CurrentOS

-- General stuff
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Maybe
import Data.Configurator
import Data.Monoid
import Control.Monad
import Prelude hiding (readFile, writeFile)

-- Stuff for XML parsing
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types
import Text.XML.Light

-- Suff for image processing
import qualified Codec.Image.DevIL as I
import Data.Array.Unboxed
import Data.Array.IArray
import GHC.Exts
import Data.Word


-- Debug stuff
--import FNIStash.File.General
--import FNIStash.File.DAT
--import qualified Data.Text.IO as T
--import Debug.Trace

-- Sets up paths, generates files, and builds the text environment
initialize messages appRoot guiRoot = do

    -- First do one time initialization stuff
    writeBMessage messages  $ Initializing $ CfgStart
    cfg <- ensureConfig appRoot

    -- Construct the DB
    writeBMessage messages $ Initializing DBStart
    conn <- initializeDB appRoot

    writeBMessage messages $ Initializing AssetsStart
    ensureGUIAssets guiRoot cfg
    writeBMessage messages $ Initializing AssetsComplete    

    writeBMessage messages $ Initializing EnvStart
    pak <- readPAKPrefixes cfg envPrefixes

-- -- This part is for writing out a particular DAT file for testing.  TODO clean up later
--    let dest = "C:\\Users\\Dan\\Desktop\\FNI Testing\\dump\\"
--        writeDatFile f d = let t = fromJust $lkupPAKFile f pak >>= return . textDAT . (runGetSuppress getDAT)
--            in T.writeFile (dest <> d) t
--
--    trace "about to write---------" $ return ()
--    writeDatFile "MEDIA/INVENTORY/BAG_ARMS_SLOT.DAT" "BAG_ARMS_SLOT.DAT"
--    trace "end write -----------" $ return ()

    -- Build the data lookup environment
    let env = buildEnv pak conn
    
    return env


{-
    The stuff here usually only does work the first time the program is run.
-}

-- Create the program directory if it doesn't exist, location at AppData/Roaming/FNIStash
ensureAppRoot = do
    appRoot <- getAppDataDirectory "FNIStash"
    createTree appRoot
    return appRoot

-- Copy the HTML assets if neceessary
ensureHtml appRoot = do
    let htmlRoot = (appRoot </> "GUI")
    htmlExists <- isDirectory htmlRoot
    if htmlExists then
        return ()
        else
            copyDirContents "GUI" htmlRoot
    return htmlRoot

-- Do I really need to roll my own copy directory function? Ugh.
copyDirContents curDir curDestDir = do
    createTree curDestDir
    curContents <- listDirectory curDir
    let curLen = length $ encodeString curDir
        curContentsNoParent = map (decodeString . (drop $ curLen + 1) . encodeString) curContents
    mapM_ (copyContentItem curDir curDestDir) curContentsNoParent

copyContentItem srcDir destDir itemName = do
    let srcPath = srcDir </> itemName
        destPath = destDir </> itemName
    isDir <- isDirectory srcPath
    if isDir then
        copyDirContents srcPath destPath
        else
            copyFile srcPath destPath

-- If the Backend.conf configuration file does not exist, write out a default one.  Load
-- whatever cfg is available at the end.
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


-- If the necessary GUI files do not exist, then generate them.
ensureGUIAssets root cfg = do
    let assetPath = root </> "GUIAssets"
    assetsExist <- isDirectory assetPath
    when (not assetsExist) $ do        
        createTree assetPath
        guiPAK <- readPAKPrefixes cfg guiAssets
        -- writeHTML will go here to write out HTML page? maybe use quasi quotes
        withAssetsContaining guiPAK ".TTF" $ writeAssetFile assetPath -- for fonts
        withAssetsContaining guiPAK "SCREENS" $ exportScreen assetPath -- for backgrounds
        withAssetsContaining guiPAK ".IMAGESET" $ processImageSet assetPath guiPAK
       

withAssetsContaining guiPAK subStr action =
    let pathDataTuples = mapsnd entryData $ M.toList $ pakWithKeysContaining subStr guiPAK
        mapsnd f list = map (\(x,y) -> (x,f y)) list
    in forM_  pathDataTuples (uncurry action)

writeAssetFile assetPath lkupPath dataBS =
     writeFile (assetPath </> (filename $ fromText lkupPath)) dataBS


-- The list of prefix paths at which the desired icons are found.
guiAssets = (fmap ("MEDIA/UI/ICONS/" <>) ["ARMOR", "FISH", "GEMS", "JEWELRY", "MISC",
                                          "POTIONS", "QUESTITEMS", "WEAPONS", "SPELLS"])
            <>
            (fmap ("MEDIA/UI/HUD/INGAMETEXTURESHEETS" <>) [""]) -- this captures all ingametexturesheets
            <>
            ["MEDIA/UI/LOADING"]
            <>
            (fmap ("MEDIA/UI/" <>) ["HUGE.TTF"])


withTempFile assetPath name contents action =
    let tempFilePath = (assetPath </> name)
    in do
        writeFile tempFilePath contents
        action tempFilePath
        fileExists <- isFile tempFilePath
        (when fileExists $ removeFile tempFilePath) :: IO ()

-- Given a desired destination path for images, a pak archive from which to extract
-- the data, and a decoded/unzipped bytestring for a file conforming to the .IMAGESET
-- XML format, will write image files to the destination path for each file listed in
-- the .IMAGESET
processImageSet assetPath pak _ iset = do
    let rootElement = flip (!!) 1 $ onlyElems $ parseXML iset
        Just imageFilePath = fmap (T.toUpper . T.pack) $ findAttr "Imagefile" rootElement
        Just imageFile = lkupPAKFile imageFilePath pak
    -- First we need to write the original DDS image to a temp file so we can use
    -- DevIL library to read in and convert.
    withTempFile assetPath (filename $ fromText imageFilePath) imageFile $ \path -> do
        I.ilInit
        dds <- I.readImage $ encodeString path
        let subImagesXML = elChildren rootElement
            subImagesData = map subImageData subImagesXML
        forM_ subImagesData $ \(name, dims) ->
            writeImageAsPNG assetPath name $ snapBox dims dds -- map over list of all sub images

-- For an XML element el describing the location and size of an icon in a larger imageset file,
-- parse the XML into a 5 tuple containing name, x, y, width, and height
subImageData el =
    let k = ( T.pack . fromJust $ findAttr "Name" el, (read . fromJust $ findAttr "XPos" el
            , read . fromJust $ findAttr "YPos" el, read . fromJust $ findAttr "Width" el
            , read . fromJust $ findAttr "Height" el))
    in k

-- Writes image data out to disk as PNG
writeImageAsPNG assetPath name newImgData =
    I.writeImage (encodeString (assetPath </> (basename $ fromText name) <.> "png")) newImgData

exportScreen assetPath name imgData =
    withTempFile assetPath (filename $ fromText name) imgData $ \tempPath -> do
        I.ilInit
        dds <- I.readImage (encodeString tempPath)
        let cropped = snapBox (0,0,1024,765) dds
        writeImageAsPNG assetPath name cropped

-- Pulls a pulls a frame out of image data
snapBox (x,y,w,h) dds = ulSubImage x y w h dds

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

-- Utility instance for defining XML attributes as string literals.
instance IsString QName where
    fromString = unqual

{-
    The stuff below here is stuff that must be initialized/used each time the program is run.
-}

-- Reads a PAK file, but restricts the data read in to only those files matching as
-- specific file path prefix, such as MEDIA/UI/ICONS
readPAKPrefixes cfg prefs = do
    pakMANFileBinary <- require cfg "MANFILE"
    man <- readPAKMAN pakMANFileBinary
    let subMan = filterMANByPrefix man prefs
    pakFileBinary <- require cfg "PAKFILE"
    pakFiles subMan pakFileBinary

-- PAK file path prefixes that contain game data needed to build the data lookup environment
envPrefixes = [ "MEDIA/EFFECTSLIST.DAT", "MEDIA/UNITS/ITEMS", "MEDIA/SKILLS", "MEDIA/INVENTORY"
              , "MEDIA/GRAPHS/STATS", "MEDIA/AFFIXES/ITEMS", "MEDIA/AFFIXES/GEMS"
              , "MEDIA/TRIGGERABLES", "MEDIA/STATS", "MEDIA/UNITS/MONSTERS/PETS"
              , "MEDIA/SPAWNCLASSES"]



