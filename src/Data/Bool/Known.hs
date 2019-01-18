{-# LANGUAGE DataKinds, KindSignatures #-}
module Data.Bool.Known where


class KnownBool (bool :: Bool) where
  boolVal :: proxy bool -> Bool

instance KnownBool 'True where
  boolVal _ = True

instance KnownBool 'False where
  boolVal _ = False
