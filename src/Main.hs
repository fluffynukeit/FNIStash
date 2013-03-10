-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import FNIStash.Logic.Config
import FNIStash.Logic.Env
import FNIStash.File.SharedStash
import FNIStash.File.General
import FNIStash.File.Crypto

import Prelude hiding (readFile)

import Filesystem
import Filesystem.Path
import Filesystem.Path.CurrentOS

import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Binary.Strict.Get
import Data.Maybe
import Data.Configurator
import Control.Monad.Reader
import Graphics.UI.Ji
import Graphics.UI.Ji.JQuery

testDir = "C:\\Users\\Dan\\Desktop\\FNI Testing"
sharedStashCrypted = testDir </> "sharedstash_v2.bin"

main = do
    items <- stashText
    serve Config
        { jiPort = 10001
        , jiRun = runJi
        , jiWorker = worker items
        , jiInitHTML = "GUI.html"
        , jiStatic = "C:\\Users\\Dan\\My Code\\FNIStash\\wwwroot"
        }

worker :: MonadJi m => T.Text -> m ()
worker items = do
    setTitle "FNIStash"
    body <- getBody
    element <- newElement "div"
    setText (T.unpack items) element
    appendTo body element
    animate element [("opacity","0")] 3000 Linear $ return ()
    
stashText = do
    cfg <- processPathsAndConfig
    env <- buildEnv cfg
    ssData <- readCryptoFile (encodeString sharedStashCrypted) >>= return . fileGameData
    let sharedStashResult = runGetWithFail "Can't read shared stash file!" (getSharedStash env) (toStrict ssData)
    return $ case sharedStashResult of
        Left error -> error
        Right sharedStash -> (runReader (ssTextOutput sharedStash) env)


ssTextOutput = textSharedStash

processPathsAndConfig = do
    -- first create the program directory if it doesn't exist
    appPath <- getAppDataDirectory "FNIStash"
    docPath <- getDocumentsDirectory
    createTree appPath
    -- now write out a config file if one does not exist
    let confPath = appPath </> "Backend.conf"
    confExists <- isFile confPath
    if confExists then
        return ()
        else
            writeConfigOut confPath $ defaultConfigOut docPath
    load [Required (encodeString confPath)]
