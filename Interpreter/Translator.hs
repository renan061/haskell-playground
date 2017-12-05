module Translator (translate) where

import LExp

translate :: LExp -> LExp

translate (LAbs x v@(LVar y)) | x == y = I
                              | otherwise = K :$: v

translate (LAbs x (e1 :$: e2)) = s (tx e1) (tx e2)
    where tx e = translate (LAbs x e)
          s (K :$: e1) (K :$: e2)   = K :$: (e1 :$: e2)
          s (K :$: e1) I            = e1
          s (K :$: e1) e2           = B :$: e1 :$: e2
          s e1 (K :$: e2)           = C :$: e1 :$: e2

translate (LAbs x l@(LAbs y e)) | x == y    = K :$: (translate l)
                                | otherwise = translate (LAbs x (translate l))

translate (LAbs x e) = K :$: e

translate e = e
