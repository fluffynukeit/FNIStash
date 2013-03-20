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

-- This file defines the Translate typeclass, which translates TL2 description strings such as
-- "This item gives [VALUE] to strength" to something like "This item gives 23 to strength"

-- Using a typeclass might be overkill.  I don't think translation happnens very often.


import Data.Monoid

class Translate a where
    -- Takes a record to translate "a", and the markup (such as "VALUE"), and returns the translation.
    translateMarkup :: a -> String -> String

-- Given a Translate instance and a sentence full of markup (like This item gives [VALUE] to strength")
-- returns a translated sentence with all markup elements replaced with their translations.
translateSentence :: Translate a => a -> String -> String
translateSentence a sent =
    let breakOn a = break (a ==)
        (pref, post1) = breakOn '[' sent
        (markup, post2) = breakOn ']' (drop 1 post1)
        suffix = drop 1 post2
        translatedMarkup = translateMarkup a markup
        newSent = pref <> translatedMarkup <> suffix
    in if null post1 then sent -- nothing left to translate
       else translateSentence a newSent

