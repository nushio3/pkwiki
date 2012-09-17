{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import Test.Feat
import Test.Feat.Class
import Data.Data

data Expr a = Add (Expr a) (Expr a) | Mul (Expr a) (Expr a) | Val a
  deriving (Eq,Show, Typeable)

instance (Enumerable a) => Enumerable (Expr a) where
  enumerate =  consts $ [fmap Val shared]
     ++ [Mul <$> shared <*> shared]
     ++ [fmap (funcurry Add) shared] -- more efficient

main = do
  print $ (index (2^8) :: [Bool])
  print $ (index (2^8) :: [Double])
  print $ (index (2^8) :: Expr ())
