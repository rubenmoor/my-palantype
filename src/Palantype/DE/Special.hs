{-|
special characters: @ / ( ) $ ...
white space: TAB SPACE RETURN
command keys: UP DOWN LEFT RIGHT PAGE UP HOME
extra: CTRL+ALT+DEL, Windows-Tap

the most common keys will correspond to the fingerspelling dictionary like this:
the first character of the key (T for TAB, D for $, ...) will serve as the
steno key for that key
-}

{-# LANGUAGE TemplateHaskell #-}

module Palantype.DE.Special where

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

data ModifierPrimary
  = ModPrimNone
  | ModPrimCtrl
  | ModPrimWin
  | ModPrimAlt

data ModifierSecondary
  = ModSecShift
  | ModSecNone

dictSpecial :: [(KIChord, Text)]
dictSpecial = do
    modP <- modifiersPrimary
    modS <- modifiersSecondary
    (plover, steno) <- keys
    pure ( parseChordDE $ RawSteno $ toStenoStr modP modS steno
         , toPloverStr modP modS plover
         )

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

keysMN :: [(Text, Text)]
keysMN =
  -- TODO move to DE.Plover
  [ ("{*-|}"        , "S") -- capitalize last word retroactively
  , ("{-|}"         , "B") -- capitalize next word
  , ("{*>}"         , "G") -- uncapitalize last word retroactively
  , ("{*?}"         , "H") -- retroactively add space
  , ("{^.\n\n^}{-|}", "D") -- paragraph
  , ("*"            , "DM") -- "* ": markdown paragraph
  , ("{*!}"         , "F") -- retroactively delete space
  , ("{^.}{-|}"     , "J") -- full stop: . w/o space and capitalize next word
  , ("{^:}{-|}"     , "JL") -- full stop: . w/o space and capitalize next word
  , ("{^?}{-|}"     , "JN") -- full stop: . w/o space and capitalize next word
  , ("{^!}{-|}"     , "JR") -- full stop: . w/o space and capitalize next word
  --  TODO: N
  , ("{^,}"         , "A") -- attachkomma
  , ("{^;}"         , "NA") -- attach semicolon
  , ("{^:}"         , "LA") -- attach colon
  , ("{^-^}"        , "H") -- hyphen to attach words
  , ("{^\t^}", "DJ")
  , ("{#BackSpace}", "B-")
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

keysExtra :: [(Text, Text)]
keysExtra =
  [ ("", "ILNSD")]
