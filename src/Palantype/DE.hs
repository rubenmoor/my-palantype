module Palantype.DE
  ( module Palantype.DE.Primitives
  , pDE
  ) where

import Palantype.DE.Primitives
import Data.Data (Proxy(Proxy))
import Palantype.DE.Keys (Key)

pDE :: Proxy Key
pDE = Proxy
