{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeOperators, UndecidableInstances #-}
module Generics.UnRep.ToTH where

import Data.Functor.Identity
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


instance KnownSymbol symbol
      => ToTH String symbol where
  toTH = toTH'String

-- | Use this instead of 'toTH' if @-Wsimplifiable-class-constraints@ complains
-- about the @ToTH String symbol@ constraint.
toTH'String :: forall proxy symbol. KnownSymbol symbol
            => proxy symbol -> TH.Q String
toTH'String _ = pure $ symbolVal (Proxy @symbol)

instance KnownSymbol symbol
      => ToTH TH.Name symbol where
  toTH = toTH'Name

-- | Use this instead of 'toTH' if @-Wsimplifiable-class-constraints@ complains
-- about the @ToTH TH.Name rep@ constraint.
toTH'Name :: forall proxy symbol. KnownSymbol symbol
          => proxy symbol -> TH.Q TH.Name
toTH'Name _ = TH.mkName <$> toTH'String (Proxy @symbol)


instance ToTH (Maybe a) 'Nothing where
  toTH _ = pure Nothing

instance ToTH a rep
      => ToTH (Maybe a) ('Just rep) where
  toTH _ = Just <$> toTH (Proxy @rep)


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


data Field f = Field
  { fieldName     :: f TH.Name
  , fieldBangType :: TH.BangType
  }

sequenceField :: Functor f
              => Field f -> f (Field Identity)
sequenceField (Field fx y) = (\x -> Field (Identity x) y) <$> fx

fieldVarBangType :: Field Identity -> TH.VarBangType
fieldVarBangType (Field (Identity x) (y, z)) = (x, y, z)

instance ( ToTH (Maybe TH.Name)       name
         , ToTH TH.SourceUnpackedness sourceUnpackedness
         , ToTH TH.SourceStrictness   sourceStrictness
         )
      => ToTH (Field Maybe)
              (Generics.S1 ('Generics.MetaSel name sourceUnpackedness sourceStrictness decidedStrictness)
                           rep)
         where
  toTH _ = Field
       <$> toTH (Proxy @name)
       <*> ( (,)
         <$> ( TH.Bang
           <$> toTH (Proxy @sourceUnpackedness)
           <*> toTH (Proxy @sourceStrictness)
             )
         <*> pure (TH.ConT ''Int)
           )

instance ToTH [Field Maybe] Generics.U1 where
  toTH _ = pure []

instance ToTH (Field Maybe) (Generics.S1 metaSel rep)
      => ToTH [Field Maybe] (Generics.S1 metaSel rep) where
  toTH _ = (:[]) <$> toTH (Proxy @(Generics.S1 metaSel rep))

instance ( ToTH [Field Maybe] rep1
         , ToTH [Field Maybe] rep2
         )
      => ToTH [Field Maybe] (rep1 Generics.:*: rep2) where
  toTH _ = (++)
       <$> toTH (Proxy @rep1)
       <*> toTH (Proxy @rep2)


instance ( KnownSymbol        name
         , ToTH [Field Maybe] rep
         )
      => ToTH TH.Con
              (Generics.C1 ('Generics.MetaCons name fixity isRecord)
                           rep)
         where
  toTH _ = do
    nameString  :: String <- pure $ symbolVal (Proxy @name)
    name        :: TH.Name       <- toTH'Name (Proxy @name)
    fieldMaybes :: [Field Maybe] <- toTH      (Proxy @rep)
    case (take 1 nameString, fieldMaybes, traverse sequenceField fieldMaybes) of
      (":", [l, r], Nothing)
        -> pure $ TH.InfixC (fieldBangType l)
                            name
                            (fieldBangType r)
      (_, _, Nothing)
        -> pure $ TH.NormalC name
                             (map fieldBangType fieldMaybes)
      (_, _, Just fields)
        -> pure $ TH.RecC name
                          (map fieldVarBangType fields)

instance ToTH [TH.Con] Generics.V1 where
  toTH _ = pure []

instance ToTH TH.Con   (Generics.C1 metaCons rep)
      => ToTH [TH.Con] (Generics.C1 metaCons rep) where
  toTH _ = (:[]) <$> toTH (Proxy @(Generics.C1 metaCons rep))

instance ( ToTH [TH.Con] rep1
         , ToTH [TH.Con] rep2
         )
      => ToTH [TH.Con] (rep1 Generics.:+: rep2) where
  toTH _ = (++)
       <$> toTH (Proxy @rep1)
       <*> toTH (Proxy @rep2)


instance ( KnownSymbol   datatypeName
         , ToTH Bool     isNewtype
         , ToTH [TH.Con] rep
         )
      => ToTH TH.Dec
              (Generics.D1 ('Generics.MetaData datatypeName moduleName packageName isNewtype) rep)
         where
  toTH _ = do
    name         :: TH.Name  <- toTH'Name (Proxy @datatypeName)
    isNewtype    :: Bool     <- toTH      (Proxy @isNewtype)
    constructors :: [TH.Con] <- toTH      (Proxy @rep)
    case (isNewtype, constructors) of
      (True, [constructor])
        -> pure $ TH.NewtypeD [] name [] Nothing constructor  []
      _ -> pure $ TH.DataD    [] name [] Nothing constructors []
