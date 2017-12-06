module Translator (translate) where

import LExp

translate :: LExp -> LExp

translate (a :$: b) = translate a :$: (translate b)

translate (LAbs x v@(LVar y)) | x == y    = I
                              | otherwise = K :$: v

translate (LAbs x (a :$: b)) = s (tx a) (tx b)
    where tx e = translate (LAbs x e)
          s (K :$: a) (K :$: b)   = K :$: (a :$: b)
          s (K :$: a) I           = a
          s (K :$: a) b           = B :$: a :$: b
          s a (K :$: b)           = C :$: a :$: b

translate (LAbs x l@(LAbs y e)) | x == y    = K :$: (translate l)
                                | otherwise = translate (LAbs x (translate l))

translate (LAbs x e) = K :$: e

translate e = e
