{-# LANGUAGE DataKinds, KindSignatures #-}
module GHC.Generics.DecidedStrictness.Known where

import GHC.Generics


class KnownDecidedStrictness (decidedStrictness :: DecidedStrictness) where
  decidedStrictnessVal :: proxy decidedStrictness -> DecidedStrictness

instance KnownDecidedStrictness 'DecidedLazy where
  decidedStrictnessVal _ = DecidedLazy

instance KnownDecidedStrictness 'DecidedStrict where
  decidedStrictnessVal _ = DecidedStrict

instance KnownDecidedStrictness 'DecidedUnpack where
  decidedStrictnessVal _ = DecidedUnpack
