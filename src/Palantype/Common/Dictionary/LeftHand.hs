{-# LANGUAGE TemplateHaskell #-}
{-|
Shared functions and data types for [LANG].Fingerspelling and Dictionary.Special,
they make both use of a right-hand mode selection and left-hand typing
-}

module Palantype.Common.Dictionary.LeftHand where

import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import Palantype.Common.TH (failure, fromJust)
import qualified Data.Text as Text
import Data.Foldable (Foldable(maximum))
import Data.Function (($))
import Palantype.Common.Class (Palantype(toKeys))
import Data.Tuple (snd)
import Data.Ord (Ord((<)))
import Palantype.DE.Keys (Key(LeftL))
import Data.Char (Char)
import qualified Data.Map.Strict as Map
import Text.Show (Show(show))

{-|

{#control(1)}
{#alt(2)}
{#super(3)}

Note that the output of {#shift(2)} depends on the keyboard layout
as configured by your system. Palantype.DE.Special contains finger spelling
for special characters too and thus offers a layout-independent alternative.

{#shift(4)}

cf. https://github.com/openstenoproject/plover/wiki/Dictionary-Format
-}
toPloverStr
  :: ModifierPrimary
  -> ModifierSecondary
  -> (Char -> Text)
  -> Char
  -> Text
toPloverStr modPrim modSec applyShift chr =
  case modPrim of
      ModPrimNone  -> case modSec of
          ModSecNone  -> "{& " <> Text.singleton chr <> "}"
          ModSecShift -> "{&" <> applyShift chr <> "}"
      ModPrimCtrl  -> mkPloverStr "control"
      ModPrimWin   -> mkPloverStr "super"
      ModPrimAlt   -> mkPloverStr "alt"
  where
    mkPloverStr commandStr =
      let (shiftStr, closing) = case modSec of
            ModSecNone -> ("", "")
            ModSecShift -> ("shift(", ")")
      in     "{#"
          <> commandStr
          <> "("
          <> shiftStr
          <> Text.singleton chr
          <> closing
          <> ")}"

toStenoStr :: Text -> ModifierPrimary -> ModifierSecondary -> Text -> Text
toStenoStr strModeSteno modPrim modSec strSteno =
    let lastLeft = maximum (toKeys $ snd $ $fromJust $ Text.unsnoc strSteno)
        stenoPrim = case modPrim of
            ModPrimNone  -> strModeSteno
            ModPrimCtrl  -> "s" <> strModeSteno
            ModPrimWin   -> "D" <> strModeSteno
            ModPrimAlt   -> "n" <> strModeSteno
        stenoSec = case modSec of
            ModSecNone  -> ""
            ModSecShift -> "S"
    in     stenoSec
        <> stenoPrim
        <> ( if lastLeft < LeftL then "-" else "" )
        <> strSteno

{-|
primary modifier keys
Ctrl, Win, and Alt
-}
data ModifierPrimary
  = ModPrimNone
  | ModPrimCtrl
  | ModPrimWin
  | ModPrimAlt

{-|
secondary modifier key
Shift
-}
data ModifierSecondary
  = ModSecNone
  | ModSecShift
