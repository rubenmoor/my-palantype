{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Palantype.Common.RawSteno.Type where

import TextShow (TextShow (showb, showt), fromText)
import Data.String (IsString (fromString))
import Text.Show (Show (show))
import Data.Eq (Eq)
import Data.Ord (Ord)
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Category ((<<<))

newtype RawSteno = RawSteno { unRawSteno :: Text }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey, Hashable, NFData)

instance TextShow RawSteno where
    showb = fromText <<< unRawSteno

instance IsString RawSteno where
    fromString = RawSteno <<< fromString

instance Show RawSteno where
    show = Text.unpack <<< showt
