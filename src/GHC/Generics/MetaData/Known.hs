{-# LANGUAGE DataKinds, FlexibleInstances, PolyKinds, ScopedTypeVariables, TypeApplications, TypeSynonymInstances #-}
module GHC.Generics.MetaData.Known where

import Data.Proxy
import GHC.TypeLits
import qualified GHC.Generics as Generics

import Data.Bool.Known


data MetaData = MetaData
  { metaDataDatatypeName :: String
  , metaDataModuleName   :: String
  , metaDataPackageName  :: String
  , metaDataIsNewtype    :: Bool
  }
  deriving (Show, Eq, Ord)


-- |
-- An alternative to 'Generics.Datatype', whose methods take a 'Generics.Rep'
-- as arguments instead of a 'Proxy'.
class KnownMetaData a where
  metaDataVal :: proxy a -> MetaData

instance ( KnownSymbol datatypeName
         , KnownSymbol moduleName
         , KnownSymbol packageName
         , KnownBool   isNewtype
         )
      => KnownMetaData ('Generics.MetaData datatypeName moduleName packageName isNewtype) where
  metaDataVal _ = MetaData (symbolVal (Proxy @datatypeName))
                           (symbolVal (Proxy @moduleName))
                           (symbolVal (Proxy @packageName))
                           (boolVal   (Proxy @isNewtype))

instance KnownMetaData metaData
      => KnownMetaData (Generics.D1 metaData rep) where
  metaDataVal _ = metaDataVal (Proxy @metaData)
