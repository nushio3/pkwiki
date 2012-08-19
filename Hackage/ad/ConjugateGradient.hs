{-# LANGUAGE Rank2Types, BangPatterns, ScopedTypeVariables #-}

module ConjugateGradient where

import Prelude hiding (all, mapM, sum)
import qualified Control.Monad.State as State
import Data.Foldable (all, toList, sum)
import Data.Traversable
import Numeric.AD.Types
import Numeric.AD.Mode.Reverse (grad)
import Numeric.AD.Internal.Composition
import Numeric.AD.Newton (extremum)


-- gradientDescent :: (Traversable f, Fractional a, Ord a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> [f a]
-- gradientDescent f x0 = go x0 fx0 xgx0 0.1 (0 :: Int)
--     where
--         (fx0, xgx0) = gradWith' (,) f x0
--         go x fx xgx !eta !i
--             | eta == 0     = [] -- step size is 0
--             | fx1 > fx     = go x fx xgx (eta/2) 0 -- we stepped too far
--             | zeroGrad xgx = [] -- gradient is 0
--             | otherwise    = x1 : if i == 10
--                                   then go x1 fx1 xgx1 (eta*2) 0
--                                   else go x1 fx1 xgx1 eta (i+1)
--             where
--                 zeroGrad = all (\(_,g) -> g == 0)
--                 x1 = fmap (\(xi,gxi) -> xi - eta * gxi) xgx
--                 (fx1, xgx1) = gradWith' (,) f x1
-- {-# INLINE gradientDescent #-}

gradientDescent :: (Traversable f, Fractional a, Ord a) =>
                (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> [f a]
gradientDescent f x0 = go x0 r0 d0
  where
    -- we internally operate with lists for performing zipWith
    zipTWith op xs ys = State.evalState (mapM zipper xs) (toList ys)
      where zipper x = do
            (y:ys) <- State.get
            State.put ys
            return (op x y)

    innerProd x y = sum $ zipTWith (*) x y

    d0 = fmap negate $ grad f x0
    r0 = d0
    go xi ri di = xi: go xi1 ri1 di1
      where
        ai  = last $ take 20 $ extremum (\a -> f $ zipTWith (+) (fmap lift xi) (fmap (a*) (fmap lift di))) 0
        xi1 = zipTWith (+) xi (fmap (ai*) di)
        ri1 = fmap negate $ grad f xi1
        bi1 = max 0 $  innerProd ri1 (zipTWith (-) ri1 ri) / innerProd ri1 ri1
        di1 = zipTWith (+) ri1 $ fmap (bi1*) di
{-# INLINE gradientDescent #-}



gradientAscent :: (Traversable f, Fractional a, Ord a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> [f a]
gradientAscent f = gradientDescent (negate . f)
{-# INLINE gradientAscent #-}