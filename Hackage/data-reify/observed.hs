{-# LANGUAGE TypeFamilies, DeriveDataTypeable, RankNTypes #-}
import Control.Applicative
import Data.Data
import Data.Generic.Schemes
import Data.Reify
import Data.Reify.Graph

-- The first step succeeds.
main = do
  (print =<<) $ reifyGraph $ parity (Var "X")
  (print =<<) $ reifyGraph $ everywhere (caster rename) $ parity (Var "X")


rename :: Bit -> Bit
rename (Var "X") = Var "Y"
rename x = x

-- apply function only when the type matches.
caster :: (Typeable a) => (a -> a) -> (forall b. Typeable b => b -> b)
caster f x =
  case (cast =<<) $ fmap f $ cast x of
    Just y  -> y
    Nothing -> x

-- Borrowed from Gill09
data Bit = Xor Bit Bit
         | Delay Bit
         | Input [Bool]
         | Var String
         deriving (Show,Typeable,Data)

data BitNode s
  = GraphXor s s
  | GraphDelay s
  | GraphInput [Bool]
  | GraphVar String
    deriving (Show,Typeable,Data)

-- Parity circuit
parity :: Bit -> Bit
parity input = output
  where
    output = Xor (Delay output) input

instance MuRef Bit where
  type DeRef Bit = BitNode
  mapDeRef f (Xor a b) = GraphXor <$> f a <*> f b
  mapDeRef f (Delay b) = GraphDelay <$> f b
  mapDeRef f (Input bs) = pure $ GraphInput bs
  mapDeRef f (Var nm)   = pure $ GraphVar nm
