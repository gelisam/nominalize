{-# LANGUAGE DataKinds, KindSignatures #-}
module GHC.Generics.DecidedStrictness.Known where

import qualified GHC.Generics as Generics
import qualified Language.Haskell.TH.Syntax as TH


class KnownDecidedStrictness (decidedStrictness :: Generics.DecidedStrictness) where
  decidedStrictnessVal :: proxy decidedStrictness -> TH.DecidedStrictness

instance KnownDecidedStrictness 'Generics.DecidedLazy where
  decidedStrictnessVal _ = TH.DecidedLazy

instance KnownDecidedStrictness 'Generics.DecidedStrict where
  decidedStrictnessVal _ = TH.DecidedStrict

instance KnownDecidedStrictness 'Generics.DecidedUnpack where
  decidedStrictnessVal _ = TH.DecidedUnpack
