{-# LANGUAGE DataKinds, FlexibleInstances, PolyKinds, ScopedTypeVariables, TypeApplications, TypeOperators, TypeSynonymInstances #-}
module GHC.Generics.MetaSel.Known where

import Data.Proxy
import qualified GHC.Generics as Generics
import qualified Language.Haskell.TH.Syntax as TH

import GHC.Generics.DecidedStrictness.Known
import GHC.Generics.SourceStrictness.Known
import GHC.Generics.SourceUnpackedness.Known
import GHC.TypeLits.Symbol.Known


data MetaSel = MetaSel
  { metaSelName               :: Maybe String
  , metaSelSourceUnpackedness :: TH.SourceUnpackedness
  , metaSelSourceStrictness   :: TH.SourceStrictness
  , metaSelDecidedStrictness  :: TH.DecidedStrictness
  }
  deriving (Show, Eq, Ord)


-- |
-- An alternative to 'Generics.Selector', whose methods take a 'Generics.Rep'
-- as arguments instead of a 'Proxy'.
class KnownMetaSel a where
  metaSelVal :: proxy a -> MetaSel

instance ( KnownMaybeSymbol        name
         , KnownSourceUnpackedness sourceUnpackedness
         , KnownSourceStrictness   sourceStrictness
         , KnownDecidedStrictness  decidedStrictness
         )
      => KnownMetaSel ('Generics.MetaSel name sourceUnpackedness sourceStrictness decidedStrictness) where
  metaSelVal _ = MetaSel (maybeSymbolVal        (Proxy @name))
                         (sourceUnpackednessVal (Proxy @sourceUnpackedness))
                         (sourceStrictnessVal   (Proxy @sourceStrictness))
                         (decidedStrictnessVal  (Proxy @decidedStrictness))

instance KnownMetaSel metaSel
      => KnownMetaSel (Generics.S1 metaSel rep) where
  metaSelVal _ = metaSelVal (Proxy @metaSel)


class KnownMetaSelList a where
  metaSelListVal :: proxy a -> [MetaSel]

instance KnownMetaSelList rep
      => KnownMetaSelList (Generics.C1 metaCons rep) where
  metaSelListVal _ = metaSelListVal (Proxy @rep)

instance KnownMetaSelList Generics.U1 where
  metaSelListVal _ = []

instance KnownMetaSel metaSel
      => KnownMetaSelList (Generics.S1 metaSel rep) where
  metaSelListVal _ = [metaSelVal (Proxy @metaSel)]

instance ( KnownMetaSelList rep1
         , KnownMetaSelList rep2
         )
      => KnownMetaSelList (rep1 Generics.:*: rep2) where
  metaSelListVal _ = metaSelListVal (Proxy @rep1)
                  ++ metaSelListVal (Proxy @rep2)


class KnownMetaSelListList a where
  metaSelListListVal :: proxy a -> [[MetaSel]]

instance KnownMetaSelListList rep
      => KnownMetaSelListList (Generics.D1 metaData rep) where
  metaSelListListVal _ = metaSelListListVal (Proxy @rep)

instance KnownMetaSelListList Generics.V1 where
  metaSelListListVal _ = []

instance KnownMetaSelList rep
      => KnownMetaSelListList (Generics.C1 metaCons rep) where
  metaSelListListVal _ = [metaSelListVal (Proxy @rep)]

instance ( KnownMetaSelListList rep1
         , KnownMetaSelListList rep2
         )
      => KnownMetaSelListList (rep1 Generics.:+: rep2) where
  metaSelListListVal _ = metaSelListListVal (Proxy @rep1)
                      ++ metaSelListListVal (Proxy @rep2)

