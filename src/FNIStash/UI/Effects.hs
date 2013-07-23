-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.UI.Effects
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

module FNIStash.UI.Effects  where


import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.Browser

flashElement outDuration el = do
    animate el [("opacity", "0")] 300 Swing
        (animate el [("opacity", "1")] outDuration Swing (return ()))

crossFade elOut elIn halfDur = do
    return elIn # setVis False # setStyle [("opacity", "0")] # unit
    fadeOut elOut halfDur Linear $ do
        return elOut # setVis False # unit
        return elIn # setVis True # unit
        fadeIn elIn halfDur Linear $ return ()


setVis v = setStyle [("visibility",
    case v of
        True    -> "inherit"
        False   -> "hidden"
        )]

setDisp v = setStyle [("display",
    case v of
        True    -> "inherit"
        False   -> "none"
        )]

setFaded v = setStyle [("opacity",
    case v of
        True    -> "0.3"
        False   -> "inherit"
        )]
