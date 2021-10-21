{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE DeriveDataTypeable #-}
module Palantype.EN.Keys where

import Palantype.Common (Palantype (..), Finger (..))
import TextShow (singleton, TextShow (..))
import qualified Data.Map as Map
import Data.Typeable (Typeable)
import Data.Data (Data)

-- the palantype.en keyboard

-- | a key on a steno keyboard
-- |
-- | The palan order: SCPTH+MFRNLYOEAUI^NLCMFRPT+SH
data Key =
    LeftS
  | LeftC
  | LeftP
  | LeftT
  | LeftH
  | LeftCross
  | LeftM
  | LeftF
  | LeftR
  | LeftN
  | LeftL
  | LeftY
  | Unused1
  | LeftO
  | LeftE
  | Unused2
  | Unused3
  | RightA
  | RightU
  | MiddleI
  | RightPoint
  | RightN
  | RightL
  | RightC
  | RightM
  | RightF
  | RightR
  | RightP
  | RightT
  | RightCross
  | RightS
  | RightH
  deriving (Eq, Ord, Typeable, Data)

instance Palantype Key where
  keyCode = \case
    LeftS      -> 'S'
    LeftC      -> 'C'
    LeftP      -> 'P'
    LeftT      -> 'T'
    LeftH      -> 'H'
    LeftCross  -> '+'
    LeftM      -> 'M'
    LeftF      -> 'F'
    LeftR      -> 'R'
    LeftN      -> 'N'
    LeftL      -> 'L'
    LeftY      -> 'Y'
    Unused1    -> '_'
    LeftO      -> 'O'
    LeftE      -> 'E'
    Unused2    -> '_'
    Unused3    -> '_'
    RightA     -> 'A'
    RightU     -> 'U'
    MiddleI    -> 'I'
    RightPoint -> '^'
    RightN     -> 'N'
    RightL     -> 'L'
    RightC     -> 'C'
    RightM     -> 'M'
    RightF     -> 'F'
    RightR     -> 'R'
    RightP     -> 'P'
    RightT     -> 'T'
    RightCross -> '+'
    RightS     -> 'S'
    RightH     -> 'H'

instance TextShow Key where
  showb = singleton . keyCode
