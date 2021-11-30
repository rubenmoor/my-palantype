{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Palantype.DE.Keys where

import           Data.Data                      ( Data )
import           Data.Eq                        ( Eq )
import qualified Data.Map                      as Map
import           Data.Ord                       ( Ord )
import           Data.Proxy                     ( Proxy(Proxy) )
import           Data.Typeable                  ( Typeable )
import           Palantype.Common               ( Finger(..)
                                                , Palantype(..)
                                                )
import           TextShow                       ( TextShow(..)
                                                , singleton
                                                )
import Control.Category ((<<<))

-- the palantype.de keyboard
--
-- S|Z   H/e  M  L       .     + N   F/W/V  -s/-er
-- B|P   D|T  J  N       .     L K/G S/Z|Tz D/T
-- G|K   F/V  W  R/er-   .     M P/B ʃ|ç    -en
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
  | RightN
  | RightKG
  | RightPB
  | RightFWVIv
  | RightSZTz
  | RightSchCh
  | RightSE
  | RightDT
  | RightEn
  deriving (Eq, Ord, Typeable, Data)

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
        RightN        -> 'N'
        RightKG       -> 'K'
        RightPB       -> 'P'
        RightFWVIv    -> 'F'
        RightSZTz     -> 'S'
        RightSchCh    -> 'ʃ' -- U+0283
        RightSE       -> 's'
        RightDT       -> 'D'
        RightEn       -> 'n'

instance TextShow Key where
    showb = singleton <<< keyCode
