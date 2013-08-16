-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.Comm.Messages
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

module FNIStash.Comm.Messages
( BMessage(..)
, InitEvent(..)
, Notice(..)
, FMessage(..)
, Messages(..)
, ItemSummary(..)
, Location(..)
, ItemClass(..)
, ItemMatch(..)
, ItemStatus(..)
, ItemsReport(..)
, ItemReport(..)
, newMessages
, onlyFMessages
, onlyBMessages
, writeFMessage
, writeBMessage
) where

import Control.Concurrent
import FNIStash.Logic.Item
import FNIStash.Logic.DB
import FNIStash.File.Variables
import Graphics.UI.Threepenny

data ItemsReport = ItemsReport
    { reportMissingItems :: [ItemReport]
    , reportPercentFound :: Float
    , reportGUIDsRegistered :: Int
    , reportGUIDsAllItems :: Int
    }
data ItemReport = ItemReport
    { reportGUID :: GUID
    , reportName :: Maybe String
    , reportRarity :: Maybe Int
    , reportLevel :: Maybe Int
    , reportCreatable :: Bool
    } 

data BMessage = Initializing InitEvent
              | LocationContents [(Location, Maybe Item)]
              | Notice Notice
              | Visibility [ItemMatch]
              | ResponseItem Element (Maybe Item)

data InitEvent = CfgStart
               | DBStart
               | AssetsStart
               | AssetsComplete
               | EnvStart
               | BackupsStart
               | ImportsStart
               | RegisterStart
               | Complete
               | InitError String
               | ArchiveDataStart
               | ArchiveData [ItemSummary]
               | ReportStart
               | ReportData ItemsReport

data Notice = Error String
            | Info String
            | Saved String

data FMessage = Move [(Location, Location)] -- order is FROM then TO
              | Save
              | Search String
              | RequestItem Element Location
              | ExportDB
              deriving Show

data Messages = Messages
    { fSource :: Chan FMessage
    , bSource :: Chan BMessage
    }

newMessages = do
    f <- newChan
    b <- newChan
    return $ Messages f b

onlyFMessages :: Messages -> IO [FMessage]
onlyFMessages (Messages f _) = getChanContents f

onlyBMessages :: Messages -> IO [BMessage]
onlyBMessages (Messages _ b) = getChanContents b

writeFMessage (Messages f _) m = writeChan f m
writeBMessage (Messages _ b) m = writeChan b m
