{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeOperators, UndecidableInstances #-}
module Generics.UnRep.ToTH where

import Data.Functor.Identity
import Data.List
import Data.Proxy
import GHC.TypeLits
import qualified Data.Typeable as Typeable
import qualified GHC.Generics as Generics
import qualified Language.Haskell.TH.Syntax as TH


class ToTH (th :: *) (rep :: k) where
  toTH :: proxy rep -> th


instance ToTH Bool 'True where
  toTH _ = True

instance ToTH Bool 'False where
  toTH _ = False


instance KnownSymbol symbol
      => ToTH String symbol where
  toTH = toTH'String

-- | Use this instead of 'toTH' if @-Wsimplifiable-class-constraints@ complains
-- about the @ToTH String symbol@ constraint.
toTH'String :: forall proxy symbol. KnownSymbol symbol
            => proxy symbol -> String
toTH'String _ = symbolVal (Proxy @symbol)

instance KnownSymbol symbol
      => ToTH TH.Name symbol where
  toTH = toTH'Name

-- | Use this instead of 'toTH' if @-Wsimplifiable-class-constraints@ complains
-- about the @ToTH TH.Name rep@ constraint.
toTH'Name :: forall proxy symbol. KnownSymbol symbol
          => proxy symbol -> TH.Name
toTH'Name _ = TH.mkName $ toTH'String (Proxy @symbol)


instance ToTH (Maybe a) 'Nothing where
  toTH _ = Nothing

instance ToTH a rep
      => ToTH (Maybe a) ('Just rep) where
  toTH _ = Just $ toTH (Proxy @rep)


instance Typeable.Typeable a
      => ToTH TH.Type (Generics.K1 i a) where
  toTH _ = go $ Typeable.typeRep (Proxy @a)
    where
      go :: Typeable.TypeRep -> TH.Type
      go typeRep_ = foldl' TH.AppT thTyCon thArgs
        where
          tyCon :: Typeable.TyCon
          tyCon = Typeable.typeRepTyCon typeRep_

          thTyCon :: TH.Type
          thTyCon = TH.ConT $ TH.mkName $ Typeable.tyConName tyCon

          args :: [Typeable.TypeRep]
          args = Typeable.typeRepArgs typeRep_

          thArgs :: [TH.Type]
          thArgs = map go args


instance ToTH TH.SourceUnpackedness 'Generics.NoSourceUnpackedness where
  toTH _ = TH.NoSourceUnpackedness

instance ToTH TH.SourceUnpackedness 'Generics.SourceNoUnpack where
  toTH _ = TH.SourceNoUnpack

instance ToTH TH.SourceUnpackedness 'Generics.SourceUnpack where
  toTH _ = TH.SourceUnpack


instance ToTH TH.SourceStrictness 'Generics.NoSourceStrictness where
  toTH _ = TH.NoSourceStrictness

instance ToTH TH.SourceStrictness 'Generics.SourceLazy where
  toTH _ = TH.SourceLazy

instance ToTH TH.SourceStrictness 'Generics.SourceStrict where
  toTH _ = TH.SourceStrict


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
         , ToTH TH.Type               rep
         )
      => ToTH (Field Maybe)
              (Generics.S1 ('Generics.MetaSel name sourceUnpackedness sourceStrictness decidedStrictness)
                           rep)
         where
  toTH _ = Field (toTH (Proxy @name))
                 ( TH.Bang (toTH (Proxy @sourceUnpackedness))
                           (toTH (Proxy @sourceStrictness))
                 , toTH (Proxy @rep)
                 )

instance ToTH [Field Maybe] Generics.U1 where
  toTH _ = []

instance ToTH (Field Maybe) (Generics.S1 metaSel rep)
      => ToTH [Field Maybe] (Generics.S1 metaSel rep) where
  toTH _ = [toTH (Proxy @(Generics.S1 metaSel rep))]

instance ( ToTH [Field Maybe] rep1
         , ToTH [Field Maybe] rep2
         )
      => ToTH [Field Maybe] (rep1 Generics.:*: rep2) where
  toTH _ = toTH (Proxy @rep1)
        ++ toTH (Proxy @rep2)


instance ( KnownSymbol        name
         , ToTH [Field Maybe] rep
         )
      => ToTH TH.Con
              (Generics.C1 ('Generics.MetaCons name fixity isRecord)
                           rep)
         where
  toTH _ = case (take 1 nameString, fieldMaybes, traverse sequenceField fieldMaybes) of
    (":", [l, r], Nothing)
      -> TH.InfixC (fieldBangType l)
                   name
                   (fieldBangType r)
    (_, _, Nothing)
      -> TH.NormalC name
                    (map fieldBangType fieldMaybes)
    (_, _, Just fields)
      -> TH.RecC name
                 (map fieldVarBangType fields)
    where
      nameString :: String
      nameString = symbolVal (Proxy @name)

      name :: TH.Name
      name = toTH'Name (Proxy @name)

      fieldMaybes :: [Field Maybe]
      fieldMaybes = toTH (Proxy @rep)


instance ToTH [TH.Con] Generics.V1 where
  toTH _ = []

instance ToTH TH.Con   (Generics.C1 metaCons rep)
      => ToTH [TH.Con] (Generics.C1 metaCons rep) where
  toTH _ = [toTH (Proxy @(Generics.C1 metaCons rep))]

instance ( ToTH [TH.Con] rep1
         , ToTH [TH.Con] rep2
         )
      => ToTH [TH.Con] (rep1 Generics.:+: rep2) where
  toTH _ = toTH (Proxy @rep1)
        ++ toTH (Proxy @rep2)


instance ( KnownSymbol   datatypeName
         , ToTH Bool     isNewtype
         , ToTH [TH.Con] rep
         )
      => ToTH TH.Dec
              (Generics.D1 ('Generics.MetaData datatypeName moduleName packageName isNewtype) rep)
         where
  toTH _ = case (isNewtype, constructors) of
      (True, [constructor])
        -> TH.NewtypeD [] name [] Nothing constructor  []
      _ -> TH.DataD    [] name [] Nothing constructors []
    where
      name :: TH.Name
      name = toTH'Name (Proxy @datatypeName)

      isNewtype :: Bool
      isNewtype = toTH (Proxy @isNewtype)

      constructors :: [TH.Con]
      constructors = toTH (Proxy @rep)
