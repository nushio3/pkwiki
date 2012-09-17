{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

import Control.Applicative
import Test.Feat
import Test.Feat.Class
import Data.Data

--  ghc -ddump-splices main-th.hs

data Expr a = Add (Expr a) (Expr a) (Expr a) | Mul (Expr a) (Expr a) | Val a | Vals [a]
  deriving (Eq,Show, Typeable)

deriveEnumerable ''Expr

-- -Ddump-splices

main = do
  print $ (index (2^8) :: [Bool])
  print $ (index (2^8) :: [Double])
  print $ (index (2^8) :: Expr ())
