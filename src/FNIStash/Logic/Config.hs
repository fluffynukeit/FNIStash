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

import FNIStash.File.General (slashTextPath)

import qualified Data.Text as T
import qualified Data.List as L
import Data.Monoid
import Filesystem
import Filesystem.Path
import Filesystem.Path.CurrentOS

data ConfigItem = Comment {comment :: T.Text }
                | Item {description:: T.Text, name :: ConfigName, value :: T.Text}

data ConfigName = MANFILE | PAKFILE | SHAREDSTASH
    deriving (Eq, Show)

-- This file contains functions for using configuration files

type ConfigOut = [ConfigItem]

defaultConfigOut docDirectory =
          [ Comment "This file configures the FNIStash backend.",
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

updateConfigOut conf add@(Comment c) = conf ++ [add]
updateConfigOut conf add@(Item d n v) =
     let maybeIndex = L.findIndex (\i -> name i == n) conf
         f (Nothing) = conf ++ [add] -- if new, just append to the end
         f (Just ind) = let (p, k:t) = L.splitAt ind conf
                      in p <> (add:t) -- if not new, replace old configname
     in f $ maybeIndex

textConfigOutList [] = []
textConfigOutList ((Comment c):xs) = ("# " <> c):(textConfigOutList xs)
textConfigOutList ((Item d n v):xs) = ("# " <> d <> "\n" <> (T.pack $ show n) <> " = " <> v):
                                 (textConfigOutList xs)

textConfigOut conf = T.concat $ L.intersperse "\n\n" $ textConfigOutList conf

-- writeConfigOut :: FilePath -> ConfigOut -> IO ()
writeConfigOut path conf = writeTextFile path $ textConfigOut conf


