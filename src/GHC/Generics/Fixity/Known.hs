{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications #-}
module GHC.Generics.Fixity.Known where

import Data.Proxy
import GHC.Generics
import GHC.TypeLits

import GHC.Generics.Associativity.Known


class KnownFixity (fixity :: FixityI) where
  fixityVal :: proxy fixity -> Fixity

instance KnownFixity 'PrefixI where
  fixityVal _ = Prefix

instance ( KnownAssociativity associativity
         , KnownNat precedence
         ) => KnownFixity ('InfixI associativity precedence) where
  fixityVal _ = Infix (associativityVal (Proxy @associativity))
                      (fromInteger $ natVal (Proxy @precedence))
