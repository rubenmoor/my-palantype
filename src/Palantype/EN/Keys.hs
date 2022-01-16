{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Palantype.EN.Keys where

import           Control.Category               ( (<<<) )
import           Data.Data                      ( Data )
import           Data.Eq                        ( Eq )
import           Data.Ord                       ( Ord (compare), comparing )
import           Palantype.Common.Class         ( Palantype(..)
                                                )
import           TextShow                       ( TextShow(..)
                                                , singleton, fromString
                                                )
import Data.Int (Int)
import GHC.Generics (Generic)
import Text.Show (Show (show))
import Text.Read (Read, readMaybe)
import Data.Aeson (FromJSON, ToJSON, ToJSONKey)
import Servant.API (ToHttpApiData (toUrlPiece), FromHttpApiData (parseUrlPiece))
import qualified Data.Text as Text
import Data.Maybe (maybe)
import Data.Either (Either(Left, Right))
import qualified Data.Map.Strict as Map
import Control.DeepSeq (NFData)

-- the palantype.en keyboard

-- | a key on a steno keyboard
--   The order of constructors matters, because of the use of constr
--   indices from typeable; the palan order is explicitly derived below via
--   palanRank
--   The Ord instance is given below via the palan order.
data Key =
    LeftC
  | LeftS
  | LeftCross
  | LeftP
  | LeftT
  | LeftH
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
  | MiddleI
  | RightU
  | RightA
  | Unused3
  | RightN
  | RightL
  | RightC
  | RightM
  | RightF
  | RightR
  | RightP
  | RightT
  | RightCross
  | RightH
  | RightS
  | RightPoint
  deriving stock (Eq, Data)

-- | The palan order: SCPTH+MFRNLYOEAUI^NLCMFRPT+SH
palanRank :: Key -> Int
palanRank = \case
  LeftS -> 1
  LeftC -> 2
  LeftP -> 3
  LeftT -> 4
  LeftH -> 5
  LeftCross -> 6
  LeftM -> 7
  LeftF -> 8
  LeftR -> 9
  LeftN -> 10
  LeftL -> 11
  LeftY -> 12
  Unused1 -> 13
  LeftO -> 14
  LeftE -> 15
  Unused2 -> 16
  Unused3 -> 17
  RightA -> 18
  RightU -> 19
  MiddleI -> 20
  RightPoint -> 21
  RightN -> 22
  RightL -> 23
  RightC -> 24
  RightM -> 25
  RightF -> 26
  RightR -> 27
  RightP -> 28
  RightT -> 29
  RightCross -> 30
  RightS -> 31
  RightH -> 32

instance Ord Key where
  compare = comparing palanRank

instance Palantype Key where
    type PatternGroup Key = Pattern

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

    toDescription = \case
      PatSimple -> "Identical letters, one single chord"
      PatSimpleMulti -> "Identical letters, multiple chords"

    patSimpleMulti = PatSimpleMulti
    lsPrimitives = []
    mapExceptions = Map.empty

instance TextShow Key where
    showb = singleton <<< keyCode

data Pattern
  = PatSimple
  | PatSimpleMulti
  deriving stock (Data, Eq, Generic, Ord, Read, Show)

instance FromJSON Pattern
instance NFData Pattern
instance ToJSON Pattern
instance ToJSONKey Pattern

instance TextShow Pattern where
  showb = fromString <<< show

instance ToHttpApiData Pattern where
  toUrlPiece = Text.pack <<< show

instance FromHttpApiData Pattern where
  parseUrlPiece =
    maybe (Left "failed to read: Pattern") Right <<< readMaybe <<< Text.unpack
