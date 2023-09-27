{-# LANGUAGE TemplateHaskell #-}
{-|
Shared functions and data types for [LANG].Fingerspelling, Dictionary.Special,
Dictionary.Numbers, and Dictionary.Commands

`FingerSpelling` and `Special` make both use of a right-hand mode selection and left-hand typing
`Numbers` and `Commands` make both use of a left-hand mode selection and right-hand typing
-}

module Palantype.Common.Dictionary.Shared where

import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import Palantype.Common.TH (fromJust, failure)
import qualified Data.Text as Text
import Data.Function (($))
import Palantype.Common.Class (Palantype(toKeys))
import Data.Tuple (snd, fst)
import Data.Ord (Ord((<), (>)))
import Palantype.DE.Keys (Key(LeftM, RightL))
import Data.Foldable (Foldable(maximum, minimum))
import Data.Maybe (fromMaybe)
import Data.Eq (Eq)

toPloverLiteralGlued :: Text -> Text
toPloverLiteralGlued str = "{&" <> str <> "}"

{-|
Turn a command string in to a command in plover format:

e.g.
{#return}
{#control(backspace)}
{#alt(shift(page_up))}

cf. https://github.com/openstenoproject/plover/wiki/Dictionary-Format
-}
toPloverCommand
  :: ModifierPrimary
  -> ModifierSecondary
  -> Text
  -> Text
toPloverCommand modPrim modSec str =
  let
      (modPrimOpen, modPrimClose) = case modPrim of
            ModPrimNone -> (""        , "" )
            ModPrimCtrl -> ("control(", ")")
            ModPrimWin  -> ("super("  , ")")
            ModPrimAlt  -> ("alt("    , ")")
      (modSecOpen, modSecClose) = case modSec of
            ModSecNone  -> (""      , "" )
            ModSecShift -> ("shift(", ")")
  in
         "{}{#" -- the leading {} tells plover to cancel any pending formatting
                -- from prior chords
      <> modPrimOpen
      <> modSecOpen
      <> str
      <> modSecClose
      <> modPrimClose
      <> "}"

toStenoStrLeftHand :: Text -> ModifierPrimary -> ModifierSecondary -> Text -> Text
toStenoStrLeftHand strModeSteno modPrim modSec strSteno =
    let
        strSteno' = Text.replace "-" "" strSteno
        lastLeft = minimum
            $ fromMaybe ($failure $ "No parse: " <> Text.unpack strSteno)
            $ toKeys $ snd $ $fromJust $ Text.unsnoc strSteno'
    in     strSteno'
        <> ( if lastLeft < LeftM then "-" else "" )
        <> strModeSteno
        <> primaryModifierToSteno modPrim
        <> secondaryModifierToSteno modSec

toStenoStrRightHand :: Text -> ModifierPrimary -> ModifierSecondary -> Text -> Text
toStenoStrRightHand strModeSteno modPrim modSec strSteno =
    let
        strSteno' = Text.replace "-" "" strSteno
        fstRight = maximum
          $ fromMaybe ($failure $ "No parse: " <> Text.unpack strSteno)
          $ toKeys $ fst $ $fromJust $ Text.uncons strSteno'
    in
           secondaryModifierToSteno modSec
        <> primaryModifierToSteno modPrim
        <> strModeSteno
        <> ( if fstRight > RightL then "-" else "" )
        <> strSteno'

{-|
primary modifier keys
Ctrl, Win, and Alt
-}
data ModifierPrimary
  = ModPrimNone
  | ModPrimCtrl
  | ModPrimWin
  | ModPrimAlt
  deriving (Eq, Ord)

{-|
secondary modifier key
Shift
-}
data ModifierSecondary
  = ModSecNone
  | ModSecShift
  deriving (Eq, Ord)

primaryModifierToSteno :: ModifierPrimary -> Text
primaryModifierToSteno = \case
    ModPrimNone  -> ""
    ModPrimCtrl  -> "ʃ"
    ModPrimWin   -> "S"
    ModPrimAlt   -> "F"

secondaryModifierToSteno :: ModifierSecondary -> Text
secondaryModifierToSteno = \case
    ModSecNone  -> ""
    ModSecShift -> "D"
