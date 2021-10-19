{-# LANGUAGE LambdaCase #-}

module Palantype.EN.Keys where

import Palantype.Common (Palantype (..), Finger (..))
import TextShow (TextShow (..))
import qualified Data.Map as Map

-- the palantype.en keyboard
--

-- a key on a steno keyboard

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
  | LeftO
  | LeftE
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
  deriving (Eq, Ord)

instance Palantype Key where
  toFinger = \case
    LeftS      -> LeftPinky
    LeftC      -> LeftPinky
    LeftP      -> LeftPinky
    LeftT      -> LeftRing
    LeftH      -> LeftRing
    LeftCross  -> LeftRing
    LeftM      -> LeftMiddle
    LeftF      -> LeftMiddle
    LeftR      -> LeftMiddle
    LeftN      -> LeftIndex
    LeftL      -> LeftIndex
    LeftY      -> LeftIndex
    LeftO      -> LeftThumb
    LeftE      -> LeftThumb
    RightA     -> RightThumb
    RightU     -> RightThumb
    MiddleI    -> RightThumb
    RightPoint -> RightIndex
    RightN     -> RightIndex
    RightL     -> RightIndex
    RightC     -> RightMiddle
    RightM     -> RightMiddle
    RightF     -> RightMiddle
    RightR     -> RightRing
    RightP     -> RightRing
    RightT     -> RightRing
    RightCross -> RightPinky
    RightS     -> RightPinky
    RightH     -> RightPinky

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
    LeftO      -> 'O'
    LeftE      -> 'E'
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

  toKeys = \case
    'S' -> [LeftS, RightS]
    'C' -> [LeftC, RightC]
    'P' -> [LeftP, RightP]
    'T' -> [LeftT, RightT]
    'H' -> [LeftH, RightH]
    '+' -> [LeftCross, RightCross]
    'M' -> [LeftM, RightM]
    'F' -> [LeftF, RightF]
    'R' -> [LeftR, RightR]
    'N' -> [LeftN, RightN]
    'L' -> [LeftL, RightL]
    'Y' -> [LeftY]
    'O' -> [LeftO]
    'E' -> [LeftE]
    'A' -> [RightA]
    'U' -> [RightU]
    'I' -> [MiddleI]
    '^' -> [RightPoint]
    _   -> []

instance TextShow Key where
  showb = showb . keyCode
