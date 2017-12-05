module TranslatorTest (testTranslator) where

import LExp
import Translator

tests =
    [
        -- Basic
        (
            LAbs "x" (LVar "x"),
            "I"
        ),
        (
            LAbs "x" (LVar "y"),
            "Ky"
        ),
        (
            LAbs "x" (LAbs "x" (LVar "y")),
            "K(Ky)"
        ),
        (
            LAbs "x" (LAbs "x" (LVar "x")),
            "KI"
        ),
        (
            LAbs "x" (LAbs "y" (LVar "x")),
            "K"
        ),
        (
            LAbs "x" (LAbs "y" (LVar "y")),
            "KI"
        ),
        (
            -- \x -> x y
            LAbs "x" ((LVar "x") :$: (LVar "y")),
            "CIy"
        ),
        -- Other tests
        (
            -- \y -> f x y
            LAbs "y" ((LVar "f") :$: (LVar "x") :$: (LVar "y")),
            "fx"
        ),
        (
            -- \x -> plus 1 x
            LAbs "x" ((LVar "plus") :$: (LVar "1") :$: (LVar "x")),
            " plus 1"
        ),
        (
            -- \x -> \y -> x y
            LAbs "x" (LAbs "y" ((LVar "x") :$: (LVar "y"))),
            "I"

        ),
        (
            -- \x -> \y -> y x
            LAbs "x" (LAbs "y" ((LVar "y") :$: (LVar "x"))),
            "CI"

        ),
        (
            -- \x -> \y -> \z -> x y z
            LAbs "x" (LAbs "y" (LAbs "z" ((LVar "x") :$: (LVar "y") :$: (LVar "z")))),
            "I"

        ),
        (
            (LVar "x") :$: (LVar "y"),
            "xy"
        ),
        (
            LVar "x",
            "x"
        ),
        (
            LAbs "x" (LAbs "y" (LVar "y")),
            "KI"

        ),
        (
            LAbs "x" (LAbs "x" (LVar "x")),
            "KI"

        ),
        (
            LAbs "x" (LVar "y" :$: (LVar "x")),
            "y"

        )
    ]

check :: [(LExp, String)] -> [String]
check l = map f (filter g l)
    where f (e, s) = show (translate e) ++ "  !=  " ++ s
          g (e, s) = show (translate e) /= s

testTranslator = aux (check tests)
    where aux [] = putStrLn "--\nOK\n--"
          aux (x:xs) = do putStrLn x; aux xs
