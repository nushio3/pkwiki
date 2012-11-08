---
format: markdown+lhs
title: How to Write a Lens for Data with Polymorphic Members?
...

- You can test this document with doctest.


> {-# LANGUAGE RankNTypes #-}
> {-# OPTIONS -Wall #-}
> data Consul = Consul
>   { speak :: Show a => a -> String }

> geisha :: Consul
> geisha = Consul { speak = (++ " dosu.") . show }

> -- | Test of speak
> --
> -- >>> speak geisha 130
> -- "130 dosu."

> main = return ()