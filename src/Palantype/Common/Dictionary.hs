{-|
Description: Common dictionary, i.e. language-independent commands
-}

{-# LANGUAGE TypeApplications #-}

module Palantype.Common.Dictionary
    ( kiUp
    , kiDown
    , kiBackUp
    , kiEnter
    , commands
    ) where

import           Control.Category               ( (<<<) )
import           Data.Eq                        ( Eq )
import           Data.Foldable                  ( Foldable(foldl') )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
                                                , Functor(fmap)
                                                )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Hashable                  ( Hashable )
import           Data.List                      ( (++) )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Tuple                     ( fst )
import           GHC.Err                        ( error )
import           Palantype.Common               ( Chord
                                                , KeyIndex
                                                , Palantype(fromIndex, keyCode)
                                                , toKeyIndices
                                                )
import           Palantype.Common.Indices       ( KIChord )
import qualified Palantype.Common.Indices      as KI
import           Palantype.Common.RawSteno      ( RawSteno(RawSteno)
                                                , parseChordLenient
                                                )
import qualified Palantype.DE.Keys             as DE
import qualified Palantype.EN.Keys             as EN
import           TextShow                       ( TextShow(showb, showt)
                                                , fromText
                                                )
import Data.Bifunctor (Bifunctor(first))

rawBackUp :: RawSteno
rawBackUp = "ULFTS"

simpleKIChord :: RawSteno -> KIChord
simpleKIChord = KI.fromChord <<< parseChordLenient @EN.Key

txtUp :: Text
txtUp = "T"

txtDown :: Text
txtDown = "F"

txtEnter :: Text
txtEnter = "E"

unmodifiedKIChord :: Text -> KIChord
unmodifiedKIChord str =
    KI.fromChord $ parseChordLenient @EN.Key $ fst $ mkModified str []

kiUp :: KIChord
kiUp = unmodifiedKIChord txtUp

kiDown :: KIChord
kiDown = unmodifiedKIChord txtDown

kiBackUp :: KIChord
kiBackUp = KI.fromChord $ parseChordLenient @EN.Key rawBackUp

kiEnter :: KIChord
kiEnter = unmodifiedKIChord txtEnter

data Modifier
  = ModShift
  | ModCtrl
  | ModAlt

mkModified :: Text -> [Modifier] -> (RawSteno, Text)
mkModified str [] = case HashMap.lookup str mapENModify of
    Just ploverCode -> (RawSteno $ str <> commandKeys, ploverCode)
    Nothing -> error $ "mkModified: not found in map: " <> Text.unpack str
    where commandKeys = "CFTS"

{-|
Common commands that can be modified with Shift, Control, Alt.
-}
mapENModify :: HashMap Text Text
mapENModify = HashMap.fromList
    [ (txtUp   , "{#up}")
    , (txtDown , "{#down}")
    , ("S"     , "{#left}")
    , ("L"     , "{#right}")
    , (txtEnter, "{#return}")
    , ("+"     , "{#tab}")
    , ("C"     , "{#home}")
    , ("N"     , "{#end}")
    , ("P"     , "{#pageup}")
    , ("M"     , "{#pagedown}")
    ]

mapEN :: [(RawSteno, Text)]
mapEN = [(rawBackUp, "=undo")]

commands :: HashMap KIChord Text
commands = HashMap.union m mModified
  where
    m = HashMap.fromList $ first simpleKIChord <$> mapEN
    mModified = HashMap.fromList $ first unmodifiedKIChord <$> HashMap.toList mapENModify
