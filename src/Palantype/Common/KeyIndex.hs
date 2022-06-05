{-|
Description : todo
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Palantype.Common.KeyIndex
    ( KeyIndex(..)
    , keyIndex
    , fromIndex
    , toKeyIndices
    ) where

import           Control.Category               ( (<<<) )
import           Data.Aeson                     ( FromJSON
                                                , FromJSONKey
                                                , ToJSON
                                                , ToJSONKey
                                                )
import           Data.Data                      ( Data(toConstr)
                                                , Proxy(Proxy)
                                                , constrIndex
                                                , fromConstr
                                                , indexConstr
                                                )
import           Data.Eq                        ( Eq )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( Functor(fmap) )
import           Data.Hashable                  ( Hashable )
import           Data.Int                       ( Int )
import           Data.Ord                       ( Ord )
import           Data.Proxied                   ( dataTypeOfProxied )
import           GHC.Num                        ( Num )
import           GHC.Enum                        ( Enum )
import           Palantype.Common.Internal      ( Chord(..) )

newtype KeyIndex = KeyIndex { unKeyIndex :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Hashable, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Num, Enum)

keyIndex :: forall key . Data key => key -> KeyIndex
keyIndex = KeyIndex <<< constrIndex <<< toConstr

fromIndex :: forall key . Data key => KeyIndex -> key
fromIndex i =
    let t = dataTypeOfProxied (Proxy :: Proxy key)
    in  fromConstr $ indexConstr t $ unKeyIndex i

toKeyIndices :: Data k => Chord k -> [KeyIndex]
toKeyIndices = fmap keyIndex <<< unChord
