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

flashElement outDuration el = do
    animate el [("opacity", "0")] 300 Swing
        (animate el [("opacity", "1")] outDuration Swing (return ()))

crossFade elOut elIn halfDur = do
    return elIn # setVis False # set style [("opacity", "0")] 
    fadeOut elOut halfDur Linear $ do
        return elOut # setVis False
        return elIn # setVis True
        fadeIn elIn halfDur Linear $ return ()


setVis v = set style [("visibility",
    case v of
        True    -> "inherit"
        False   -> "hidden"
        )]

