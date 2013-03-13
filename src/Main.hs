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


import FNIStash.Logic.Initialize

import qualified Data.Text as T

import Graphics.UI.Ji
import Graphics.UI.Ji.JQuery

main = do
    items <- stashText
    print items
--    serve Config
--        { jiPort = 10001
--        , jiRun = runJi
--        , jiWorker = worker items
--        , jiInitHTML = "GUI.html"
--        , jiStatic = "C:\\Users\\Dan\\My Code\\FNIStash\\wwwroot"
--        }

worker :: MonadJi m => T.Text -> m ()
worker items = do
    setTitle "FNIStash"
    body <- getBody
    element <- newElement "div"
    setText (T.unpack items) element
    appendTo body element
    animate element [("opacity","0")] 3000 Linear $ return ()
    
