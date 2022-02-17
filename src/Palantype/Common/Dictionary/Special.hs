{-|
fingerspelling: access to all the letters of the alphabet individually
-}

{-# LANGUAGE TemplateHaskell #-}

module Palantype.Common.Dictionary.Special
    (
    ) where

import Data.Text (Text)
import Data.Semigroup (Semigroup((<>)))
import Palantype.Common.Indices (KIChord, parseChordDE)
import Control.Applicative (Applicative(pure))
import Palantype.Common.Class (RawSteno(RawSteno), Palantype (toKeys))
import Data.Function (($))
import qualified Data.Text as Text
import Data.Foldable (Foldable(maximum))
import Palantype.Common.TH (fromJust)
import Data.Tuple (snd)
import Data.Ord (Ord((<)))
import Palantype.DE.Keys (Key(LeftL))

keys :: [(Text, Text)]
keys =
  [ ("a", "A")
  , ("á", "BA")
  , ("à", "JA")
  , ("b", "B-")
  , ("c", "GDM-")
  , ("ç", "GDJ-")
  , ("d", "D-")
  , ("e", "E")
  , ("é", "BE")
  , ("ê", "DE")
  , ("è", "JE")
  , ("f", "F-")
  , ("g", "G-")
  , ("h", "H-")
  , ("i", "I")
  , ("j", "J")
  , ("k", "GD-")
  , ("l", "L-")
  , ("m", "M-")
  , ("n", "N-")
  , ("o", "O")
  , ("ô", "DO")
  , ("p", "BD-")
  , ("q", "GDW-")
  , ("r", "R")
  , ("s", "S-")
  , ("t", "BD-")
  , ("u", "U")
  , ("v", "FW")
  , ("w", "W")
  , ("x", "GDM-")
  , ("y", "ÄI")
  , ("z", "SHM-")
  , ("ä", "Ä")
  , ("ö", "ÄO")
  , ("ü", "Ü")
  , ("ß", "GFW-")
  ]

data ModifierPrimary
  = ModPrimNone
  | ModPrimCtrl
  | ModPrimWin
  | ModPrimAlt

data ModifierSecondary
  = ModSecShift
  | ModSecNone

dictFingerSpelling :: [(KIChord, Text)]
dictFingerSpelling = do
    modP <- modifiersPrimary
    modS <- modifiersSecondary
    (plover, steno) <- keys
    pure ( parseChordDE $ RawSteno $ toStenoStr modP modS steno
         , toPloverStr modP modS plover
         )
  where
    {-|

    {#Control_L(0)}
    {#Alt_L(0)}
    {#Super_L(0)}

    cf. https://github.com/openstenoproject/plover/wiki/Dictionary-Format

    {& } is needed to have plover suppress spaces between chords
    -}
    toPloverStr :: ModifierPrimary -> ModifierSecondary -> Text -> Text
    toPloverStr prim sec str = case prim of
        ModPrimNone  -> case sec of
            ModSecNone  -> "{&" <> str <> "}"
            ModSecShift -> "{&" <> Text.toUpper str <> "}"
        ModPrimCtrl  -> "{#control(" <> secStr <> ")}"
        ModPrimWin   -> "{#super("   <> secStr <> ")}"
        ModPrimAlt   -> "{#alt("     <> secStr <> ")}"
      where
        secStr = case sec of
          ModSecNone -> str
          ModSecShift -> "shift(" <> str <> ")"

    toStenoStr :: ModifierPrimary -> ModifierSecondary -> Text -> Text
    toStenoStr prim sec strSteno =
           strSteno
        <> if lastLeft < LeftL then "-" else ""
        <> case prim of
               ModPrimNone  -> "L"  <> strSec
               ModPrimCtrl  -> "L"  <> strSec <> "s"
               ModPrimWin   -> "L"  <> strSec <> "D"
               ModPrimAlt   -> "L"  <> strSec <> "n"
      where
        strSec = case sec of
          ModSecNone  -> ""
          ModSecShift -> "S"
        lastLeft = maximum (toKeys $ snd $ $fromJust $ Text.unsnoc strSteno)

modifiersPrimary :: [ModifierPrimary]
modifiersPrimary =
    [ ModPrimNone
    , ModPrimCtrl
    , ModPrimWin
    , ModPrimAlt
    ]

modifiersSecondary :: [ModifierSecondary]
modifiersSecondary =
  [ ModSecNone
  , ModSecShift
  ]
