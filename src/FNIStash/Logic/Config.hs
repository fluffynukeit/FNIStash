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
    updateConfigOut,
    ConfigName(..)
) where

-- This file contains utilities for writing out a configuration file into the Configurator format
-- because Configurator doesn't have this already (!).

import Data.Monoid
import qualified Data.Text as T
import qualified Data.List as L
import qualified System.IO as I


-- Filesystem stuff
import Filesystem
import Filesystem.Path
import Filesystem.Path.CurrentOS


-- A configuration file entry (config item) is either a comment, or it's a value entry with a
-- description, key name, and key value
data ConfigItem = Comment {comment :: String }
                | Item {description:: String, name :: ConfigName, value :: String}

-- Use a data type deriving Show to get extra type safey for our config keys
data ConfigName = MANFILE | PAKFILE | SHAREDSTASHLOCATION | SHAREDSTASHFILE | DBLOCATION
    deriving (Eq, Show)

-- A configuration file is a list of config items.
type ConfigOut = [ConfigItem]


-- Defines the default config file.
defaultConfigOut docDirectory =
          [ Comment "This file configures the FNIStash backend.",
            Comment $ slashPath "Please ensure all file paths are defined using double back slashes (\\) and surrounded by quotes (\")",
            Item "The location of the TL2 PAK.MAN file, which describes the PAK asset file."
                 MANFILE $
                 slashPath "\"C:\\Program Files (x86)\\Steam\\steamapps\\common\\Torchlight II\\PAKS\\DATA.PAK.MAN\"",
            Item "The location of the TL2 PAK archive that contains game data and assets."
                 PAKFILE $
                 slashPath "\"C:\\Program Files (x86)\\Steam\\steamapps\\common\\Torchlight II\\PAKS\\DATA.PAK\"",
            Item "A directory containing the shared stash file somewhere withing its subdirectory hierarchy."
                 SHAREDSTASHLOCATION $
                 slashPath $ "\"" <> (encodeString $ docDirectory </> "My Games\\Runic Games\\Torchlight 2\\save") <> "\"",
            Item "The file name of the shared stash file."
                SHAREDSTASHFILE "\"sharedstash_v2.bin\"",
            Comment "The parameter below is optional. You should only set it to support an unusual use case.",
            Comment ("The absolute path of the folder containing the fnistash.db file.  Usually it is in AppData\\FNIStash.\n\
                    \# DBLOCATION = \"" <> slashPath "C:\\Example\\Abs\\Path\\AppData\\FNIStash\"")
          ]

-- Utility method for transforming normal slashes to \\.  Configurator doesn't like normal slashes in strings.
slashPath :: String -> String
slashPath t = T.unpack $ T.replace "\\" "\\\\" $ T.pack t -- no built in string replace?

-- Update a config file.  Any comments are simply appended to the end.  For Items, the item is
-- appended to the end of the Item does not exist.  Otherwise, the first instance of the item is
-- repaced with the update.
updateConfigOut conf add@(Comment c) = conf ++ [add]
updateConfigOut conf add@(Item d n v) =
     let maybeIndex = L.findIndex (\i -> name i == n) conf
         f (Nothing) = conf ++ [add] -- if new, just append to the end
         f (Just ind) = let (p, k:t) = splitAt ind conf
                      in p <> (add:t) -- if not new, replace old configname
     in f maybeIndex

-- Transforms a Config file into a list of text lines.  This is a helper function.
showConfigOutList :: ConfigOut -> [String]
showConfigOutList [] = []
showConfigOutList ((Comment c):xs) = ("# " <> c):(showConfigOutList xs)
showConfigOutList ((Item d n v):xs) = ("# " <> d <> "\n" <> (show n) <> " = " <> v):
                                 (showConfigOutList xs)

-- Transform a Config file into a String value.
showConfigOut :: ConfigOut -> String
showConfigOut conf = L.concat $ L.intersperse "\n\n" $ showConfigOutList conf

-- Writes the Config file to the specified path.
writeConfigOut path conf = I.writeFile (encodeString path) $ showConfigOut conf


