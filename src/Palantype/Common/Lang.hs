{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Palantype.Common.Lang
    ( Lang (..)
    ) where

import           Data.Aeson                     ( FromJSON
                                                , FromJSONKey
                                                , ToJSON
                                                , ToJSONKey
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( Eq )
import           Data.Function                  ( ($) )
import           Data.Ord                       ( Ord )
import           Data.Semigroup                 ( (<>) )
import           GHC.Generics                   ( Generic )
import           Servant.API                    ( FromHttpApiData(parseUrlPiece)
                                                , ToHttpApiData(toUrlPiece)
                                                )
import           Text.Read                      ( Read )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow ( showb)
                                                , fromText
                                                )

data Lang
  = EN
  | DE
  deriving stock (Eq, Generic, Ord, Read, Show)

instance FromJSON Lang
instance ToJSON Lang
instance FromJSONKey Lang
instance ToJSONKey Lang

instance TextShow Lang where
    showb = \case
        EN -> fromText "Palantype"
        DE -> fromText "Palantype DE"

instance ToHttpApiData Lang where
    toUrlPiece = \case
        EN -> "EN"
        DE -> "DE"

instance FromHttpApiData Lang where
    parseUrlPiece = \case
        "EN" -> Right EN
        "DE" -> Right DE
        str  -> Left $ "url piece: " <> str <> ": no parse"
