-- The purpose of this module is to define a type, 'Other.String', which is
-- easy to confuse with another type, 'Prelude.String', in order to test that
-- 'makeUnRep' picks the correct one.
module Examples.Fake where

import Prelude hiding (String)

-- |
-- Let's also use @String@ as the data constructor name, to make it extra
-- confusing.
data String = String  deriving Show
