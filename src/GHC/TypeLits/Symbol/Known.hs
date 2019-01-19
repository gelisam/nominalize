{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications #-}
module GHC.TypeLits.Symbol.Known where

import Data.Proxy
import GHC.TypeLits


class KnownMaybeSymbol (maybeSymbol :: Maybe Symbol) where
  maybeSymbolVal :: proxy maybeSymbol -> Maybe String

instance KnownMaybeSymbol 'Nothing where
  maybeSymbolVal _ = Nothing

instance KnownSymbol symbol
      => KnownMaybeSymbol ('Just symbol) where
  maybeSymbolVal _ = Just (symbolVal (Proxy @symbol))
