{-# LANGUAGE ScopedTypeVariables #-}

module Palantype.Common.RenderPlover
    ( renderPlover
    ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Palantype.Common.Class (RawSteno, Palantype)
import Palantype.Common.Internal (Chord)
import qualified Palantype.Common.RawSteno as Raw
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Functor ((<&>), (<$>), void, Functor ((<$)))
import Data.Function (($))
import TextShow (TextShow(showt))
import Text.Parsec (runParser, Parsec, char, modifyState, setState, getState, try, many1, noneOf, optional, eof)
import Data.Either
import Control.Monad (MonadPlus(mzero), Monad ((>>=)), guard)
import Data.Foldable (Foldable(null))
import Control.Applicative (Applicative(pure), Alternative ((<|>)))
import Data.Char (Char)
import Data.Maybe
import Data.Monoid ((<>), Monoid(mconcat))

-- TODO: bytestring trie
-- TODO: properly deal with spaces
renderPlover :: Palantype key => Map RawSteno Text -> [Chord key] -> Text
renderPlover map chords =
    let parts = chords <&> \c ->
            let raw = Raw.fromChord c
                strPlover = Map.findWithDefault (" " <> showt raw <> " ") raw map
            in  parsePlover strPlover
    in  mconcat parts

data Token
    = TokenCurlyOpen

parsePlover :: Text -> Text
parsePlover str = case runParser ploverString [] "" str of
    Left _    -> " PLOVER_ERR "
    Right res -> res

ploverString :: Parsec Text [Token] Text
ploverString = do
    optional glue
    ls <- many1 $     (Nothing <$  curlyClose )
                  <|> (Just    <$> backslash  )
                  <|> (Just    <$> noneOf "\\")

    -- make sure there is no input left
    eof
    -- ... and any open curly brace is closed
    allClosed

    pure $ Text.pack $ catMaybes ls

glue :: Parsec Text [Token] ()
glue = try $ do
    curlyOpen
    void $ char '&'

curlyOpen :: Parsec Text [Token] ()
curlyOpen = do
    _ <- char '{'
    modifyState (TokenCurlyOpen :)

curlyClose :: Parsec Text [Token] ()
curlyClose = try $ do
    _ <- char '}'
    getState >>= \case
        TokenCurlyOpen : rem -> setState rem
        _                    -> mzero

allClosed :: Parsec Text [Token] ()
allClosed = do
    st <- getState
    guard $ null st

backslash :: Parsec Text [Token] Char
backslash = try $ do
    _ <- char '\\'
    char '\\'
