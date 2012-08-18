{-# LANGUAGE Rank2Types, BangPatterns, ScopedTypeVariables #-}

module ConjugateGradient where

import Prelude hiding (all)
import Data.Foldable (all)
import Data.Traversable
import Numeric.AD.Types
import Numeric.AD.Mode.Reverse (grad)
import Numeric.AD.Internal.Composition
import Numeric.AD.Newton (findZero)


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

gradientDescent :: (Traversable f, Fractional a, Ord a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> [f a]
gradientDescent f x0 = go x0 r0 d0
  where
    d0 = fmap negate $ grad f x0
    r0 = d0
    go :: (Traversable f, Fractional a, Ord a) => f a ->  f a ->  f a ->  [f a]
    go xi ri di = xi: go xi ri di
      where
        ai = (!!10) $ findZero (\a -> f (fmap (a*) (sequenceA $ fmap lift di))) xi
{-# INLINE gradientDescent #-}



gradientAscent :: (Traversable f, Fractional a, Ord a) => (forall s. Mode s => f (AD s a) -> AD s a) -> f a -> [f a]
gradientAscent f = gradientDescent (negate . f)
{-# INLINE gradientAscent #-}