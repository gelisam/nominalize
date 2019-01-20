{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Generics.Unrep.ToTH where

import Data.Proxy
import GHC.TypeLits

import qualified GHC.Generics as Generics
import qualified Language.Haskell.TH.Syntax as TH


class ToTH (th :: *) (rep :: k) where
  toTH :: proxy rep -> TH.Q th


instance ToTH Bool 'True where
  toTH _ = pure True

instance ToTH Bool 'False where
  toTH _ = pure False


instance KnownNat rep
      => ToTH Integer rep where
  toTH _ = pure $ natVal (Proxy @rep)

instance ToTH Integer rep
      => ToTH Int rep where
  toTH _ = fromInteger <$> toTH (Proxy @rep)


instance KnownSymbol symbol
      => ToTH String symbol where
  toTH _ = pure $ symbolVal (Proxy @symbol)


instance ToTH (Maybe a) 'Nothing where
  toTH _ = pure Nothing

instance ToTH a rep
      => ToTH (Maybe a) ('Just rep) where
  toTH _ = Just <$> toTH (Proxy @rep)


instance ToTH TH.FixityDirection 'Generics.LeftAssociative where
  toTH _ = pure TH.InfixL

instance ToTH TH.FixityDirection 'Generics.RightAssociative where
  toTH _ = pure TH.InfixR

instance ToTH TH.FixityDirection 'Generics.NotAssociative where
  toTH _ = pure TH.InfixN


instance ToTH (Maybe TH.Fixity) 'Generics.PrefixI where
  toTH _ = pure Nothing

instance ( ToTH TH.FixityDirection associativity
         , ToTH Int                precedence
         ) => ToTH (Maybe TH.Fixity) ('Generics.InfixI associativity precedence) where
  toTH _ = Just <$> ( TH.Fixity
                  <$> toTH (Proxy @precedence)
                  <*> toTH (Proxy @associativity)
                    )


instance ToTH TH.SourceUnpackedness 'Generics.NoSourceUnpackedness where
  toTH _ = pure TH.NoSourceUnpackedness

instance ToTH TH.SourceUnpackedness 'Generics.SourceNoUnpack where
  toTH _ = pure TH.SourceNoUnpack

instance ToTH TH.SourceUnpackedness 'Generics.SourceUnpack where
  toTH _ = pure TH.SourceUnpack


instance ToTH TH.SourceStrictness 'Generics.NoSourceStrictness where
  toTH _ = pure TH.NoSourceStrictness

instance ToTH TH.SourceStrictness 'Generics.SourceLazy where
  toTH _ = pure TH.SourceLazy

instance ToTH TH.SourceStrictness 'Generics.SourceStrict where
  toTH _ = pure TH.SourceStrict


instance ToTH TH.DecidedStrictness 'Generics.DecidedLazy where
  toTH _ = pure TH.DecidedLazy

instance ToTH TH.DecidedStrictness 'Generics.DecidedStrict where
  toTH _ = pure TH.DecidedStrict

instance ToTH TH.DecidedStrictness 'Generics.DecidedUnpack where
  toTH _ = pure TH.DecidedUnpack


data MetaSel = MetaSel
  { metaSelName               :: Maybe String
  , metaSelSourceUnpackedness :: TH.SourceUnpackedness
  , metaSelSourceStrictness   :: TH.SourceStrictness
  , metaSelDecidedStrictness  :: TH.DecidedStrictness
  }
  deriving (Show, Eq, Ord)

instance ( ToTH (Maybe String)        name
         , ToTH TH.SourceUnpackedness sourceUnpackedness
         , ToTH TH.SourceStrictness   sourceStrictness
         , ToTH TH.DecidedStrictness  decidedStrictness
         )
      => ToTH MetaSel ('Generics.MetaSel name sourceUnpackedness sourceStrictness decidedStrictness) where
  toTH _ = MetaSel
       <$> toTH (Proxy @name)
       <*> toTH (Proxy @sourceUnpackedness)
       <*> toTH (Proxy @sourceStrictness)
       <*> toTH (Proxy @decidedStrictness)

instance ToTH MetaSel metaSel
      => ToTH MetaSel (Generics.S1 metaSel rep) where
  toTH _ = toTH (Proxy @metaSel)


instance ToTH [MetaSel] rep
      => ToTH [MetaSel] (Generics.C1 metaCons rep) where
  toTH _ = toTH (Proxy @rep)

instance ToTH [MetaSel] Generics.U1 where
  toTH _ = pure []

instance ToTH MetaSel metaSel
      => ToTH [MetaSel] (Generics.S1 metaSel rep) where
  toTH _ = (:[]) <$> toTH (Proxy @metaSel)

instance ( ToTH [MetaSel] rep1
         , ToTH [MetaSel] rep2
         )
      => ToTH [MetaSel] (rep1 Generics.:*: rep2) where
  toTH _ = (++)
       <$> toTH (Proxy @rep1)
       <*> toTH (Proxy @rep2)


instance ToTH [[MetaSel]] rep
      => ToTH [[MetaSel]] (Generics.D1 metaData rep) where
  toTH _ = toTH (Proxy @rep)

instance ToTH [[MetaSel]] Generics.V1 where
  toTH _ = pure []

instance ToTH [MetaSel] rep
      => ToTH [[MetaSel]] (Generics.C1 metaCons rep) where
  toTH _ = (:[]) <$> toTH (Proxy @rep)

instance ( ToTH [[MetaSel]] rep1
         , ToTH [[MetaSel]] rep2
         )
      => ToTH [[MetaSel]] (rep1 Generics.:+: rep2) where
  toTH _ = (++)
       <$> toTH (Proxy @rep1)
       <*> toTH (Proxy @rep2)


data MetaCons = MetaCons
  { metaConsName     :: String
  , metaConsFixity   :: Maybe TH.Fixity
  , metaConsIsRecord :: Bool
  }
  deriving (Show, Eq, Ord)

instance ( KnownSymbol            name
         , ToTH (Maybe TH.Fixity) fixity
         , ToTH Bool              isRecord
         )
      => ToTH MetaCons ('Generics.MetaCons name fixity isRecord) where
  toTH _ = MetaCons
       <$> pure (symbolVal (Proxy @name))
       <*> toTH (Proxy @fixity)
       <*> toTH (Proxy @isRecord)

instance ToTH MetaCons metaCons
      => ToTH MetaCons (Generics.C1 metaCons rep) where
  toTH _ = toTH (Proxy @metaCons)


instance ToTH [MetaCons] rep
      => ToTH [MetaCons] (Generics.D1 metaData rep) where
  toTH _ = toTH (Proxy @rep)

instance ToTH [MetaCons] Generics.V1 where
  toTH _ = pure []

instance ToTH MetaCons metaCons
      => ToTH [MetaCons] (Generics.C1 metaCons rep) where
  toTH _ = (:[]) <$> toTH (Proxy @metaCons)

instance ( ToTH [MetaCons] rep1
         , ToTH [MetaCons] rep2
         )
      => ToTH [MetaCons] (rep1 Generics.:+: rep2) where
  toTH _ = (++)
       <$> toTH (Proxy @rep1)
       <*> toTH (Proxy @rep2)


data MetaData = MetaData
  { metaDataDatatypeName :: String
  , metaDataModuleName   :: String
  , metaDataPackageName  :: String
  , metaDataIsNewtype    :: Bool
  }
  deriving (Show, Eq, Ord)

instance ( KnownSymbol datatypeName
         , KnownSymbol moduleName
         , KnownSymbol packageName
         , ToTH Bool   isNewtype
         )
      => ToTH MetaData ('Generics.MetaData datatypeName moduleName packageName isNewtype) where
  toTH _ = MetaData <$> pure (symbolVal (Proxy @datatypeName))
                    <*> pure (symbolVal (Proxy @moduleName))
                    <*> pure (symbolVal (Proxy @packageName))
                    <*> toTH      (Proxy @isNewtype)

instance ToTH MetaData metaData
      => ToTH MetaData (Generics.D1 metaData rep) where
  toTH _ = toTH (Proxy @metaData)
