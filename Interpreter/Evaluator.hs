module Evaluator (evaluate) where

import Error (evaluatorError)
import LExp
import Translator

eval :: LExp -> [LExp]
eval = eval' . spine
    where eval' (I:a:tail)     = aux (a, tail)
          eval' (K:a:_:tail)   = aux (a, tail)
          eval' (B:f:g:x:tail) = aux (f :$: (g :$: x),  tail)
          eval' (C:f:g:x:tail) = aux (f :$: x :$: g, tail)
          eval' a              = a
          aux (e, tail)        = eval' (spine e ++ tail)

spine :: LExp -> [LExp]
spine (a :$: b) = spine a ++ [b]
spine a = [a]

evaluate :: LExp -> String
evaluate lce = foldr (++) "" exps
    where exps = map (show . check) (eval lce)
          check v@(LVar _) = v
          check e = evaluatorError e
