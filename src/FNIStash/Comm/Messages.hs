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

module FNIStash.Comm.Messages (
    Message (..),
    BMessage (..),
    module Control.Concurrent
) where

import Control.Concurrent
import FNIStash.File.Item


data BMessage = Initializing {initStatus :: String}
              | Initialized
              | LocationContents {location :: Location, locContents :: Maybe Item}
              | Error {errorStatus :: String}

data Message a = Message a
