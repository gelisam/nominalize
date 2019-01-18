{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleInstances, KindSignatures, ScopedTypeVariables, TypeApplications, TypeSynonymInstances #-}
module Generics.Unrep where

import Data.Proxy
import GHC.TypeLits
import GHC.Generics
import Language.Haskell.TH


class KnownBool (bool :: Bool) where
  boolVal :: proxy bool -> Bool

instance KnownBool 'True where
  boolVal _ = True

instance KnownBool 'False where
  boolVal _ = False


class KnownMetaData (rep :: * -> *) where
  datatypeNameVal :: proxy rep -> String
  moduleNameVal   :: proxy rep -> String
  packageNameVal  :: proxy rep -> String
  isNewtypeVal    :: proxy rep -> Bool

instance ( KnownSymbol name
         , KnownSymbol moduleName
         , KnownSymbol packageName
         , KnownBool   isNewtype
         )
      => KnownMetaData (D1 ('MetaData name moduleName packageName isNewtype) rep) where
  datatypeNameVal _ = symbolVal (Proxy @name)
  moduleNameVal   _ = symbolVal (Proxy @moduleName)
  packageNameVal  _ = symbolVal (Proxy @packageName)
  isNewtypeVal    _ = boolVal   (Proxy @isNewtype)


-- |
-- >>> :set -XDeriveGeneric
-- >>> :set -XTemplateHaskell
-- >>> :set -XTypeApplications
-- >>> import GHC.Generics
-- >>> :{
-- data Tree = Leaf | Branch Tree Tree deriving Generic
-- makeUnrep @(Rep Tree)
-- :}
--
-- >>> :info Tree2
-- data Tree2 = Leaf2 | Branch2 Tree2 Tree2
-- ...
makeUnrep :: forall rep. KnownMetaData rep => Q [Dec]
makeUnrep = do
  typeName <- newName (datatypeNameVal (Proxy @rep) ++ "2")
  ctorName1 <- newName "Leaf2"
  ctorName2 <- newName "Branch2"

  defaultBang <- bang noSourceUnpackedness noSourceStrictness

  let typeParameters :: [TyVarBndr]
      typeParameters = []

      ctor1 :: Con
      ctor1 = NormalC ctorName1 []

      ctor2 :: Con
      ctor2 = NormalC ctorName2 [ (defaultBang, ConT typeName)
                                , (defaultBang, ConT typeName)
                                ]

      constructors :: [Con]
      constructors = [ctor1, ctor2]

      dec :: Dec
      dec = DataD [] typeName typeParameters Nothing constructors []

  pure [dec]
