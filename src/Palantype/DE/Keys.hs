{-# LANGUAGE LambdaCase #-}

module Palantype.DE.Keys where

import Palantype.Common (Palantype (..), Finger (..))
import TextShow (TextShow (..))
import qualified Data.Map as Map


-- the palantype.de keyboard
--
-- S|Z   H/e  M  L       .     + N   F/W/V  R/-er
-- B|P   D|T  J  N       .     L K/G S/Z|Tz -en
-- G|K   F/V  W  R/er-   .     M P/B ʃ|ç    D/T
-- thumbg     ^  E  A ː  . O I U

-- a key on a steno keyboard
data Key
  = LeftSZ
  | LeftBP
  | LeftGK
  | LeftHE
  | LeftDT
  | LeftFV
  | LeftM
  | LeftJ
  | LeftW
  | LeftL
  | LeftN
  | LeftREr
  | LeftDiacritic
  | LeftE
  | LeftA
  | LeftStretch
  | RightOStretch
  | RightI
  | RightU
  | RightModifier
  | RightL
  | RightM
  | RightN
  | RightKG
  | RightPB
  | RightFWVIv
  | RightSZTz
  | RightSchCh
  | RightEEr
  | RightEn
  | RightDT
  deriving (Eq, Ord)

instance Palantype Key where
  toFinger = \case
    LeftSZ        -> LeftPinky
    LeftBP        -> LeftPinky
    LeftGK        -> LeftPinky
    LeftHE        -> LeftRing
    LeftDT        -> LeftRing
    LeftFV        -> LeftRing
    LeftM         -> LeftMiddle
    LeftJ         -> LeftMiddle
    LeftW         -> LeftMiddle
    LeftL         -> LeftIndex
    LeftN         -> LeftIndex
    LeftREr       -> LeftIndex
    LeftDiacritic -> LeftThumb
    LeftE         -> LeftThumb
    LeftA         -> LeftThumb
    LeftStretch   -> LeftThumb
    RightOStretch -> RightThumb
    RightI        -> RightThumb
    RightU        -> RightThumb
    RightModifier -> RightIndex
    RightL        -> RightIndex
    RightM        -> RightIndex
    RightN        -> RightMiddle
    RightKG       -> RightMiddle
    RightPB       -> RightMiddle
    RightFWVIv    -> RightRing
    RightSZTz     -> RightRing
    RightSchCh    -> RightRing
    RightEEr      -> RightPinky
    RightEn       -> RightPinky
    RightDT       -> RightPinky

  keyCode = \case
    LeftSZ        -> 'S'
    LeftBP        -> 'B'
    LeftGK        -> 'G'
    LeftHE        -> 'H'
    LeftDT        -> 'D'
    LeftFV        -> 'F'
    LeftM         -> 'M'
    LeftJ         -> 'J'
    LeftW         -> 'W'
    LeftL         -> 'L'
    LeftN         -> 'N'
    LeftREr       -> 'R'
    LeftDiacritic -> '^'
    LeftE         -> 'E'
    LeftA         -> 'A'
    LeftStretch   -> '~'
    RightOStretch -> 'O'
    RightI        -> 'I'
    RightU        -> 'U'
    RightModifier -> '+'
    RightL        -> 'L'
    RightM        -> 'M'
    RightN        -> 'N'
    RightKG       -> 'K'
    RightPB       -> 'P'
    RightFWVIv    -> 'F'
    RightSZTz     -> 'S'
    RightSchCh    -> 'ʃ' -- U+0283
    RightEEr      -> 'e'
    RightEn       -> 'n'
    RightDT       -> 'D'

  toKeys = \case
    'S' -> [LeftSZ, RightSZTz]
    'B' -> [LeftBP]
    'G' -> [LeftGK]
    'H' -> [LeftHE]
    'D' -> [LeftDT, RightDT]
    'F' -> [LeftFV, RightFWVIv]
    'M' -> [LeftM, RightM]
    'J' -> [LeftJ]
    'W' -> [LeftW]
    'L' -> [LeftL, RightL]
    'N' -> [LeftN, RightN]
    'R' -> [LeftREr]
    '^' -> [LeftDiacritic]
    'E' -> [LeftE]
    'A' -> [LeftA]
    '~' -> [LeftStretch]
    'O' -> [RightOStretch]
    'I' -> [RightI]
    'U' -> [RightU]
    '+' -> [RightModifier]
    'K' -> [RightKG]
    'P' -> [RightPB]
    'ʃ' -> [RightSchCh]
    'e' -> [RightEEr]
    'n' -> [RightEn]
    _   -> []

instance TextShow Key where
  showb = showb . keyCode
