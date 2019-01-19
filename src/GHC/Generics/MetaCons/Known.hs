{-# LANGUAGE DataKinds, FlexibleInstances, PolyKinds, ScopedTypeVariables, TypeApplications, TypeOperators, TypeSynonymInstances #-}
module GHC.Generics.MetaCons.Known where

import Data.Proxy
import GHC.TypeLits
import qualified GHC.Generics as Generics

import Data.Bool.Known
import GHC.Generics.Fixity.Known


data MetaCons = MetaCons
  { metaConsName     :: String
  , metaConsFixity   :: Generics.Fixity
  , metaConsIsRecord :: Bool
  }
  deriving (Show, Eq, Ord)


-- |
-- An alternative to 'Generics.Constructor', whose methods take a 'Generics.Rep'
-- as arguments instead of a 'Proxy'.
class KnownMetaCons a where
  metaConsVal :: proxy a -> MetaCons

instance ( KnownSymbol name
         , KnownFixity fixity
         , KnownBool   isRecord
         )
      => KnownMetaCons ('Generics.MetaCons name fixity isRecord) where
  metaConsVal _ = MetaCons (symbolVal (Proxy @name))
                           (fixityVal (Proxy @fixity))
                           (boolVal   (Proxy @isRecord))

instance KnownMetaCons metaCons
      => KnownMetaCons (Generics.C1 metaCons rep) where
  metaConsVal _ = metaConsVal (Proxy @metaCons)


class KnownMetaConsList a where
  metaConsListVal :: proxy a -> [MetaCons]

instance KnownMetaConsList rep
      => KnownMetaConsList (Generics.D1 metaData rep) where
  metaConsListVal _ = metaConsListVal (Proxy @rep)

instance KnownMetaConsList Generics.V1 where
  metaConsListVal _ = []

instance KnownMetaCons metaCons
      => KnownMetaConsList (Generics.C1 metaCons rep) where
  metaConsListVal _ = [metaConsVal (Proxy @metaCons)]

instance ( KnownMetaConsList rep1
         , KnownMetaConsList rep2
         )
      => KnownMetaConsList (rep1 Generics.:+: rep2) where
  metaConsListVal _ = metaConsListVal (Proxy @rep1)
                   ++ metaConsListVal (Proxy @rep2)
