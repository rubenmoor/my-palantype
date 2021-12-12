{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Palantype.DE.Keys where

import           Control.Category               ( (<<<) )
import           Data.Data                      ( Data )
import           Data.Eq                        ( Eq )
import           Data.Ord                       ( Ord )
import           Palantype.Common               ( Palantype(..) )
import           TextShow                       ( TextShow(..)
                                                , singleton
                                                )

-- the palantype.de keyboard
--
-- S|Z   H/e  M  L       .     + K/G   F/W/V  -s/-er
-- B|P   D|T  J  N       .     L N     S/Z|Tz D/T
-- G|K   F/V  W  R/er-   .     M P/B   ʃ|ç    -en
-- thumb      Ä  E  A ~  . U I O Ü

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
  | LeftAUmlaut
  | LeftE
  | LeftA
  | LeftStretch
  | RightU
  | RightI
  | RightOStretch
  | RightUUmlaut
  | RightModifier
  | RightL
  | RightM
  | RightKG
  | RightN
  | RightPB
  | RightFWVIv
  | RightSZTz
  | RightSchCh
  | RightSE
  | RightDT
  | RightEn
  deriving stock (Eq, Ord, Data)

instance Palantype Key where

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
        LeftAUmlaut   -> 'Ä'
        LeftE         -> 'E'
        LeftA         -> 'A'
        LeftStretch   -> '~'
        RightU        -> 'U'
        RightI        -> 'I'
        RightOStretch -> 'O'
        RightUUmlaut  -> 'Ü'
        RightModifier -> '+'
        RightL        -> 'L'
        RightM        -> 'M'
        RightKG       -> 'K'
        RightN        -> 'N'
        RightPB       -> 'P'
        RightFWVIv    -> 'F'
        RightSZTz     -> 'S'
        RightSchCh    -> 'ʃ' -- U+0283
        RightSE       -> 's'
        RightDT       -> 'D'
        RightEn       -> 'n'

instance TextShow Key where
    showb = singleton <<< keyCode
