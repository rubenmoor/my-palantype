{-|
Description : todo
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Palantype.Common.KeyIndex
    ( KeyIndex (..)
    , keyIndex
    , fromIndex
    , toKeyIndices
    ) where

import           Control.Category               ( (<<<) )
import           Data.Eq                        ( Eq )
import           Data.Functor                   ( Functor(fmap)
                                                )
import           Data.Hashable                  ( Hashable )
import           Palantype.Common.Internal      ( Chord (..)
                                                )
import Data.Function (($))
import Data.Int (Int)
import Data.Ord (Ord)
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import GHC.Num (Num)
import Data.Data (Proxy (Proxy), Data (toConstr), constrIndex, fromConstr, indexConstr)
import Data.Proxied (dataTypeOfProxied)

newtype KeyIndex = KeyIndex { unKeyIndex :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Hashable, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Num)

keyIndex :: forall key . Data key => key -> KeyIndex
keyIndex = KeyIndex <<< constrIndex <<< toConstr

fromIndex :: forall key . Data key => KeyIndex -> key
fromIndex i =
  let t = dataTypeOfProxied (Proxy :: Proxy key)
  in  fromConstr $ indexConstr t $ unKeyIndex i

toKeyIndices :: Data k => Chord k -> [KeyIndex]
toKeyIndices = fmap keyIndex <<< unChord
