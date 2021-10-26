{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Palantype.DE.Keys where

import Palantype.Common (Palantype (..), Finger (..))
import TextShow (singleton, TextShow (..))
import qualified Data.Map as Map
import Data.Proxy (Proxy(Proxy))
import Data.Data (Data)
import Data.Typeable (Typeable)

-- the palantype.de keyboard
--
-- S|Z   H/e  M  L       .     + N   F/W/V  R/-er
-- B|P   D|T  J  N       .     L K/G S/Z|Tz -en
-- G|K   F/V  W  R/er-   .     M P/B ʃ|ç    D/T
-- thumb      ^  E  A ~  . _ O I U

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
  | RightEEr
  | RightEn
  | RightDT
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
    RightEEr      -> 'e'
    RightEn       -> 'n'
    RightDT       -> 'D'

instance TextShow Key where
  showb = singleton . keyCode
