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

module FNIStash.Comm.Messages where

import Control.Concurrent
import FNIStash.File.Item

data BMessage = Initializing String
              | Initialized
              | LocationContents Location (Maybe Item)
              | Registered [Location]
              | Notice Notice
              | Visibility [(String, Bool)]

data Notice = Error String
            | Info String
            | Saved String

data FMessage = Move {moveFrom :: Location, moveTo :: Location}
              | Save
              | Search String

data Message = FMessage FMessage
             | BMessage BMessage

type Messages = Chan Message

newMessages = newChan
dupMessages = dupChan


onlyFMessages :: Messages -> IO [FMessage]
onlyFMessages c = do
    m <- getChanContents c
    return $ map stripF $ filter isFMessage m

onlyBMessages :: Messages -> IO [BMessage]
onlyBMessages c = do
    m <- getChanContents c
    return $ map stripB $ filter isBMessage m

isFMessage (FMessage _) = True
isFMessage _ = False

isBMessage (BMessage _) = True
isBMessage _ = False

stripF (FMessage x) = x
stripB (BMessage x) = x

writeFMessage c m = writeChan c (FMessage m)
writeBMessage c m = writeChan c (BMessage m)
