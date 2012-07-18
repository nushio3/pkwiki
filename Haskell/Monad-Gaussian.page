---
format: markdown+lhs
title: Monad-Gaussian
...

We have learned that nondeterministic computation is described as List Monad in Haskell. How about continuous probabilistic distributions? Is it possible to describe them as Monads? Let's try.
 
> import Control.Monad

Let's start with some limitation; we restrict ourselves to Gaussian distributions and assume that the standard deviations are small compared to the scales we deal with.

> data Gaussian a = G {mean :: a, devi :: a}

> instance Monad Gaussian where

>   return a = G a 0
