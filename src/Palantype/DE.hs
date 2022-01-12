module Palantype.DE
  ( pDE
  ) where

import Data.Data (Proxy(Proxy))
import Palantype.DE.Keys (Key)

pDE :: Proxy Key
pDE = Proxy
