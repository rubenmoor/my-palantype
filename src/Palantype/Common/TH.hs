{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Palantype.Common.TH
  ( readLoc
  , fromJust
  , failure
  ) where

import Language.Haskell.TH (Exp, Q)
import Debug.Trace.LocationTH (failure)
import Data.Maybe (fromMaybe)

{-|
   like Text.read but relying on TemplateHaskell to provide
   location information (file/line number) upon failure.

   `readLoc` is similar to `$Debug.Trace.LocationTH.check read`,
   but uses `readMaybe` and does not need `unsafePerformIO`.
-}
readLoc :: Q Exp
readLoc =
    [| $fromJust <<< readMaybe |]

{-|
   like Data.Maybe.fromJust but relying on TemplateHaskell to provide
   location information (file/line number) when the pattern match fails.
-}
fromJust :: Q Exp
fromJust = [| fromMaybe ($failure "fromJust: Nothing") |]
