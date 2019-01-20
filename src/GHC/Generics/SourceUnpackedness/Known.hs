{-# LANGUAGE DataKinds, KindSignatures #-}
module GHC.Generics.SourceUnpackedness.Known where

import qualified GHC.Generics as Generics
import qualified Language.Haskell.TH.Syntax as TH


class KnownSourceUnpackedness (sourceUnpackedness :: Generics.SourceUnpackedness) where
  sourceUnpackednessVal :: proxy sourceUnpackedness -> TH.SourceUnpackedness

instance KnownSourceUnpackedness 'Generics.NoSourceUnpackedness where
  sourceUnpackednessVal _ = TH.NoSourceUnpackedness

instance KnownSourceUnpackedness 'Generics.SourceNoUnpack where
  sourceUnpackednessVal _ = TH.SourceNoUnpack

instance KnownSourceUnpackedness 'Generics.SourceUnpack where
  sourceUnpackednessVal _ = TH.SourceUnpack
