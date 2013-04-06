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


data BMessage = Initializing {initStatus :: String}
              | Initialized
              | LocationContents {location :: Location, locContents :: Maybe Item}
              | Error {errorStatus :: String}

data FMessage = Move {moveFrom :: Location, moveTo :: Location}

data Message = FMessage FMessage
             | BMessage BMessage

type Messages = Chan Message

newMessages = newChan

onlyFMessages :: Chan Message -> IO [FMessage]
onlyFMessages c = do
    m <- getChanContents c
    return $ map stripF $ filter isFMessage m

onlyBMessages :: Chan Message -> IO [BMessage]
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
