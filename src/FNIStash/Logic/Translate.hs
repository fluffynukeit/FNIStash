-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.Logic.Translate
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

module FNIStash.Logic.Translate (
    Translate(..),
    translateSentence
) where


import qualified Data.Text as T
import Data.Monoid

class Translate a where
    translateMarkup :: a -> T.Text -> T.Text


translateSentence :: Translate a => a -> T.Text -> T.Text
translateSentence a sent =
    let (pref, post1) = T.breakOn "[" sent
        (markup, post2) = T.breakOn "]" (T.drop 1 post1)
        suffix = T.drop 1 post2
        translatedMarkup = translateMarkup a markup
        newSent = pref <> translatedMarkup <> suffix
    in if T.null post1 then sent -- nothing left to translate
       else translateSentence a newSent

