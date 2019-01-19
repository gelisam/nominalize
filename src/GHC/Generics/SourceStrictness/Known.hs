{-# LANGUAGE DataKinds, KindSignatures #-}
module GHC.Generics.SourceStrictness.Known where

import GHC.Generics


class KnownSourceStrictness (sourceStrictness :: SourceStrictness) where
  sourceStrictnessVal :: proxy sourceStrictness -> SourceStrictness

instance KnownSourceStrictness 'NoSourceStrictness where
  sourceStrictnessVal _ = NoSourceStrictness

instance KnownSourceStrictness 'SourceLazy where
  sourceStrictnessVal _ = SourceLazy

instance KnownSourceStrictness 'SourceStrict where
  sourceStrictnessVal _ = SourceStrict
