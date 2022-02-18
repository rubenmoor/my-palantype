{-# LANGUAGE TemplateHaskell #-}
{-|
Shared functions and data types for Dictionary.Numbers and Dictionary.Commands,
they make both use of a left-hand mode selection and
right-hand numpad/command group
-}

module Palantype.Common.Dictionary.RightHand where

import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import Palantype.Common.TH (fromJust)
import qualified Data.Text as Text
import Data.Foldable (Foldable(maximum))
import Data.Function (($))
import Palantype.Common.Class (Palantype(toKeys))
import Data.Tuple (fst)
import Data.Ord (Ord((>=)))
import Palantype.DE.Keys (Key(RightFWVIv))

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
toPloverStr :: ModifierPrimary -> ModifierSecondary -> Text -> Text
toPloverStr modPrim modSec str =
    case modPrim of
        ModPrimNone  -> case modSec of
            ModSecNone  -> "{#" <> str <> "}"
            ModSecShift -> "{#shift(" <> str <> ")}"
        ModPrimCtrl  -> mkPloverStr "control"
        ModPrimWin   -> mkPloverStr "super"
        ModPrimAlt   -> mkPloverStr "alt"
  where
    mkPloverStr commandStr =
        let (shiftStr, closing) = case modSec of
                ModSecNone  -> ("", "")
                ModSecShift -> ("shift(", ")")
        in     "{#"
            <> commandStr
            <> "("
            <> shiftStr
            <> str
            <> closing
            <> ")}"

toStenoStr :: Text -> ModifierPrimary -> ModifierSecondary -> Text -> Text
toStenoStr strModeSteno modPrim modSec strSteno =
    let fstRight = maximum $ toKeys $ fst $ $fromJust $ Text.uncons strSteno
        stenoPrim = case modPrim of
            ModPrimNone  -> strModeSteno
            ModPrimCtrl  -> "H" <> strModeSteno
            ModPrimWin   -> "D" <> strModeSteno
            ModPrimAlt   -> "F" <> strModeSteno
        stenoSec = case modSec of
            ModSecNone  -> ""
            ModSecShift -> "B"
    in     stenoSec
        <> stenoPrim
        <> ( if fstRight >= RightFWVIv then "-" else "" )
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
