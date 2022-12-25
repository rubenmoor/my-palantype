module Palantype.EN
    ( pEN
    , module Palantype.EN.Keys
    ) where

import Data.Proxy (Proxy(Proxy))
import Palantype.EN.Keys (Key)

pEN :: Proxy Key
pEN = Proxy
