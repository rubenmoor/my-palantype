module Palantype.DE
  ( pDE
  , module Palantype.DE.Keys
  ) where

import Data.Data (Proxy(Proxy))
import Palantype.DE.Keys

pDE :: Proxy Key
pDE = Proxy
