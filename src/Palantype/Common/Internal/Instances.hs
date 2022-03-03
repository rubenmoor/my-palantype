{-# OPTIONS_GHC -Wno-orphans #-}
module Palantype.Common.Internal.Instances where

import TextShow (TextShow (showb), singleton)
import Palantype.Common.Internal (Series (..), Chord)
import qualified Palantype.Common.RawSteno as Raw
import Control.Category ((<<<))
import Data.Monoid (Monoid(mconcat))
import Data.List (intersperse)
import Data.Functor (Functor(fmap))
import Palantype.Common.Class (Palantype)

instance Palantype k => TextShow (Chord k) where
    showb = showb <<< Raw.fromChord

instance Palantype k => TextShow (Series k) where
    showb = mconcat <<< intersperse (singleton '/') <<< fmap showb <<< unSeries
