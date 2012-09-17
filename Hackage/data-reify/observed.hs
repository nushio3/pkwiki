{-# LANGUAGE TypeFamilies, DeriveDataTypeable, RankNTypes #-}
import Control.Applicative
import Data.Data
import Data.Generics.Schemes
import Data.Reify
import Data.Reify.Graph

-- The sharing observation succeeds for the first version and
-- prints out the 3-node graph.
-- It fails for the
main = do
  (print =<<) $ reifyGraph $ myTree (Var "X")
  (print =<<) $ reifyGraph $ everywhere' (caster modify) $ myTree (Var "X")


modify :: Expr -> Expr
modify (Val i) = Val (i+2)
modify x = x

-- apply function only when the type matches.
caster :: (Typeable a) => (a -> a) -> (forall b. Typeable b => b -> b)
caster f x =
  case (cast =<<) $ fmap f $ cast x of
    Just y  -> y
    Nothing -> x

-- Borrowed from Gill09
data Expr = Add Expr Expr
         | Mul Expr Expr
         | Val Int
         | Var String
         deriving (Show,Typeable,Data)

data ExprNode s
  = GraphAdd s s
  | GraphMul s s
  | GraphVal Int
  | GraphVar String
    deriving (Show,Typeable,Data)

-- my computation is represented as trees with sharings
myTree :: Expr -> Expr
myTree input = output
  where
    output = Mul z z
    z = Add y y
    y = Mul (Val 40) input

instance MuRef Expr where
  type DeRef Expr = ExprNode
  mapDeRef f (Add a b) = GraphAdd <$> f a <*> f b
  mapDeRef f (Mul a b) = GraphMul <$> f a <*> f b
  mapDeRef f (Val bs)  = pure $ GraphVal bs
  mapDeRef f (Var nm)  = pure $ GraphVar nm
