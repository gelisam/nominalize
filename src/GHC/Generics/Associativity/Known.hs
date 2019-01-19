{-# LANGUAGE DataKinds, KindSignatures #-}
module GHC.Generics.Associativity.Known where

import GHC.Generics


class KnownAssociativity (associativity :: Associativity) where
  associativityVal :: proxy associativity -> Associativity

instance KnownAssociativity 'LeftAssociative where
  associativityVal _ = LeftAssociative

instance KnownAssociativity 'RightAssociative where
  associativityVal _ = RightAssociative

instance KnownAssociativity 'NotAssociative where
  associativityVal _ = NotAssociative
