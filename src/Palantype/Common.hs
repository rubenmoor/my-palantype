module Palantype.Common
  ( module Palantype.Common.Class
  , module Palantype.Common.Internal
  , module Palantype.Common.Primitives
  , module Palantype.Common.Indices
  , module Palantype.Common.Dictionary
  , module Palantype.Common.RawSteno
  , MapStenoWordTake100
  ) where

import Palantype.Common.Class
import Palantype.Common.Internal
import Palantype.Common.Primitives
import Palantype.Common.Indices hiding (toKeys, fromChord)
import Palantype.Common.RawSteno
import Palantype.Common.Dictionary
import Data.Map.Strict (Map)
import Data.Int (Int)
import Data.Text (Text)

type MapStenoWordTake100 key =
  Map (PatternGroup key) (Map Greediness (Int, [(Text, RawSteno)]))
