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
        stenoPrim = case modPrim of
            ModPrimNone  -> ""
            ModPrimCtrl  -> "n"
            ModPrimWin   -> "D"
            ModPrimAlt   -> "s"
        stenoSec = case modSec of
            ModSecNone  -> ""
            ModSecShift -> "S"
    in     strSteno'
        <> ( if lastLeft < LeftM then "-" else "" )
        <> strModeSteno
        <> stenoSec
        <> stenoPrim

toStenoStrRightHand :: Text -> ModifierPrimary -> ModifierSecondary -> Text -> Text
toStenoStrRightHand strModeSteno modPrim modSec strSteno =
    let
        strSteno' = Text.replace "-" "" strSteno
        fstRight = maximum
          $ fromMaybe ($failure $ "No parse: " <> Text.unpack strSteno)
          $ toKeys $ fst $ $fromJust $ Text.uncons strSteno'
        stenoPrim = case modPrim of
            ModPrimNone  -> ""
            ModPrimCtrl  -> "v"
            ModPrimWin   -> "D"
            ModPrimAlt   -> "b"
        stenoSec = case modSec of
            ModSecNone  -> ""
            ModSecShift -> "S"
    in     stenoPrim
        <> stenoSec
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

{-|
secondary modifier key
Shift
-}
data ModifierSecondary
  = ModSecNone
  | ModSecShift
