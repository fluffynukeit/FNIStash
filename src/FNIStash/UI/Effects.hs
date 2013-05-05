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

import FNIStash.UI.Layout

flashElement outDuration (el, id) = do
    animate el [("opacity", "0")] 300 Swing
        (animate el [("opacity", "1")] outDuration Swing (return ()))
