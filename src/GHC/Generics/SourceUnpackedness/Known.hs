{-# LANGUAGE DataKinds, KindSignatures #-}
module GHC.Generics.SourceUnpackedness.Known where

import GHC.Generics


class KnownSourceUnpackedness (sourceUnpackedness :: SourceUnpackedness) where
  sourceUnpackednessVal :: proxy sourceUnpackedness -> SourceUnpackedness

instance KnownSourceUnpackedness 'NoSourceUnpackedness where
  sourceUnpackednessVal _ = NoSourceUnpackedness

instance KnownSourceUnpackedness 'SourceNoUnpack where
  sourceUnpackednessVal _ = SourceNoUnpack

instance KnownSourceUnpackedness 'SourceUnpack where
  sourceUnpackednessVal _ = SourceUnpack
