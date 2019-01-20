{-# LANGUAGE DataKinds, KindSignatures #-}
module GHC.Generics.SourceStrictness.Known where

import qualified GHC.Generics as Generics
import qualified Language.Haskell.TH.Syntax as TH


class KnownSourceStrictness (sourceStrictness :: Generics.SourceStrictness) where
  sourceStrictnessVal :: proxy sourceStrictness -> TH.SourceStrictness

instance KnownSourceStrictness 'Generics.NoSourceStrictness where
  sourceStrictnessVal _ = TH.NoSourceStrictness

instance KnownSourceStrictness 'Generics.SourceLazy where
  sourceStrictnessVal _ = TH.SourceLazy

instance KnownSourceStrictness 'Generics.SourceStrict where
  sourceStrictnessVal _ = TH.SourceStrict
