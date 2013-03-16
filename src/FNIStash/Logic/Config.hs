-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.Logic.Config
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

module FNIStash.Logic.Config (
    defaultConfigOut,
    writeConfigOut,
    updateConfigOut
) where

-- This file contains utilities for writing out a configuration file into the Configurator format
-- because Configurator doesn't have this already (!).

-- General stuff
import qualified Data.Text as T
import qualified Data.List as L
import Data.Monoid

-- Filesystem stuff
import Filesystem
import Filesystem.Path
import Filesystem.Path.CurrentOS


-- A configuration file entry (config item) is either a comment, or it's a value entry with a
-- description, key name, and key value
data ConfigItem = Comment {comment :: T.Text }
                | Item {description:: T.Text, name :: ConfigName, value :: T.Text}

-- Use a data type deriving Show to get extra type safey for our config keys
data ConfigName = MANFILE | PAKFILE | SHAREDSTASH
    deriving (Eq, Show)

-- A configuration file is a list of config items.
type ConfigOut = [ConfigItem]


-- Defines the default config file.
defaultConfigOut docDirectory =
          [ Comment "This file configures the FNIStash backend.",
            Comment $ slashTextPath "Please ensure all file paths are defined using double back slashes (\\)",
            Item "The location of the TL2 PAK.MAN file, which describes the PAK asset file."
                 MANFILE $
                 slashTextPath "\"C:\\Program Files (x86)\\Steam\\steamapps\\common\\Torchlight II\\PAKS\\DATA.PAK.MAN\"",
            Item "The location of the TL2 PAK archive that contains game data and assets."
                 PAKFILE $
                 slashTextPath "\"C:\\Program Files (x86)\\Steam\\steamapps\\common\\Torchlight II\\PAKS\\DATA.PAK\"",
            Item "The location of the shared stash file."
                 SHAREDSTASH $
                 slashTextPath $ "\"" <> (either (id) (id) $ toText $ docDirectory </> "My Games\\Runic Games\\Torchlight 2\\save") <> "\""
          ]

-- Utility method for transforming normal slashes to \\.  Configurator doesn't like normal slashes in strings.
slashTextPath :: T.Text -> T.Text
slashTextPath t = T.replace "\\" "\\\\" t

-- Update a config file.  Any comments are simply appended to the end.  For Items, the item is
-- appended to the end of the Item does not exist.  Otherwise, the first instance of the item is
-- repaced with the update.
updateConfigOut conf add@(Comment c) = conf ++ [add]
updateConfigOut conf add@(Item d n v) =
     let maybeIndex = L.findIndex (\i -> name i == n) conf
         f (Nothing) = conf ++ [add] -- if new, just append to the end
         f (Just ind) = let (p, k:t) = L.splitAt ind conf
                      in p <> (add:t) -- if not new, replace old configname
     in f maybeIndex

-- Transforms a Config file into a list of text lines.  This is a helper function.
textConfigOutList [] = []
textConfigOutList ((Comment c):xs) = ("# " <> c):(textConfigOutList xs)
textConfigOutList ((Item d n v):xs) = ("# " <> d <> "\n" <> (T.pack $ show n) <> " = " <> v):
                                 (textConfigOutList xs)

-- Transform a Config file into a T.Text value.
textConfigOut conf = T.concat $ L.intersperse "\n\n" $ textConfigOutList conf

-- Writes the Config file to the specified path.
writeConfigOut path conf = writeTextFile path $ textConfigOut conf


