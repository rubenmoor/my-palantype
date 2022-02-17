{-|
special characters for finger spelling: @ / ( ) $ ...

the most common keys will correspond to the fingerspelling dictionary like this:
the first character of the key (D for $, P for %, B for `) will serve as the
steno key for that key
-}

{-# LANGUAGE TemplateHaskell #-}

module Palantype.DE.Special
  ( dictSpecial
  )
  where

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
import Palantype.DE.Keys (Key(LeftL, LeftM))

keysMN :: [(Text, Text)]
keysMN =
  [ ("c", "GDM-")
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

dictSpecial :: [(KIChord, Text)]
dictSpecial = do
    modP <- modifiersPrimary
    modS <- modifiersSecondary
    (plover, steno) <- keysMN
    pure ( parseChordDE $ RawSteno $ toStenoStr modP modS steno
         , toPloverStr modP modS plover
         )

data ModifierPrimary
  = ModPrimNone
  | ModPrimCtrl
  | ModPrimWin
  | ModPrimAlt

data ModifierSecondary
  = ModSecShift
  | ModSecNone

{-|

{#Control_L(0)}
{#Alt_L(0)}
{#Super_L(0)}

cf. https://github.com/openstenoproject/plover/wiki/Dictionary-Format
-}
toPloverStr :: ModifierPrimary -> ModifierSecondary -> Text -> Text
toPloverStr prim sec str = case prim of
    ModPrimNone  -> case sec of
        ModSecNone -> str
        ModSecShift -> "{#shift(" <> str <> ")}"
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
    <> if lastLeft < LeftM then "-" else ""
    <> case prim of
           ModPrimNone  -> "MN"  <> strSec
           ModPrimCtrl  -> "MN"  <> strSec <> "s"
           ModPrimWin   -> "MN"  <> strSec <> "D"
           ModPrimAlt   -> "MN"  <> strSec <> "n"
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
