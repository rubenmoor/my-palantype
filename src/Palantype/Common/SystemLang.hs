{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Palantype.Common.SystemLang
    ( SystemLang (..)
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

data SystemLang
  = SystemEN
  | SystemDE
  deriving stock (Eq, Generic, Ord, Read, Show)

instance FromJSON SystemLang
instance ToJSON SystemLang
instance FromJSONKey SystemLang
instance ToJSONKey SystemLang

instance TextShow SystemLang where
    showb = \case
        SystemEN -> fromText "Palantype"
        SystemDE -> fromText "Palantype DE"

instance ToHttpApiData SystemLang where
    toUrlPiece = \case
        SystemEN -> "SystemEN"
        SystemDE -> "SystemDE"

instance FromHttpApiData SystemLang where
    parseUrlPiece = \case
        "SystemEN" -> Right SystemEN
        "SystemDE" -> Right SystemDE
        str  -> Left $ "url piece: " <> str <> ": no parse"
