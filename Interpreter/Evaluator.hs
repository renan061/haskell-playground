module Evaluator (evaluate) where

import LExp
import Translator

eval :: LExp -> [LExp]
eval = eval' . spine
    where eval' (I:a:tail)          = aux a tail
          eval' (K:a:_:tail)        = aux a tail
          eval' (S:f:g:x:tail)      = aux (f :$: x :$: (g :$: x))  tail
          eval' (B:f:g:x:tail)      = aux (f :$: (g :$: x))  tail
          eval' (C:f:g:x:tail)      = aux (f :$: x :$: g) tail
          eval' (v@(LVar _):b:tail) = v : aux b tail
          eval' v@[(LVar _)]        = v
          eval' a                   = a
          aux e tail                = eval' (spine e ++ tail)

spine :: LExp -> [LExp]
spine (a :$: b) = spine a ++ [b]
spine a = [a]

evaluate :: LExp -> String
evaluate lce = foldr (++) "" exps
    where exps = map show (eval lce)
