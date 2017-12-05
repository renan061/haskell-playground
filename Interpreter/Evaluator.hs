module Evaluator (evaluate) where

import LExp
import Translator

-- eval :: LExp -> String
-- eval (I :$: a)       = eval a
-- eval (K :$: a :$: b) = eval a
-- eval (S :$: f :$: g :$: a) = eval (f :$: a :$: (g :$: a))
-- eval (B :$: f :$: g :$: a) = eval (f :$: (g :$: a))
-- eval (C :$: f :$: a :$: b) = eval (f :$: b :$: a)
-- eval a = show a

eval :: [LExp] -> String
eval (I:a)     = eval a
eval (K:a:b)   = eval [a]
eval (S:f:g:a) = eval (f : a ++ (g:a))
eval (B:f:g:a) = eval (f : g : a)
eval (C:f:a:b) = eval (f : b : a)
eval a         = show a

-- eval (S :$: _) = error "S should not be a valid combinator"

spine :: LExp -> [LExp]
spine (a :$: b) = spine a ++ [b]
spine a = [a]

evaluate :: LExp -> String
evaluate e = eval (spine (translate e))
