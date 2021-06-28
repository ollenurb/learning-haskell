module CustomGen where

import Test.Hspec
import Test.QuickCheck

-- Ex. 1
data Fool = Fulse
          | Frue
          deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = oneof [return Fulse, return Frue]

-- Ex. 2
foolGen2 :: Gen Fool
foolGen2 = frequency [(2, return Fulse), (1, return Frue)]
