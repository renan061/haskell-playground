import Data.Char (isLower)

-- ==================================================
--
--  Types
--
-- ==================================================

type Name = String

data LExp = LVar Name
          | LExp :$: LExp
          | LAbs Name LExp
          | I
          | K
          | S
          | B
          | C

instance Show LExp where
    show (LVar n) | length n == 1 = n
                  | otherwise = " " ++ n ++ " "
    show (l1@(LVar _) :$: l2@(LVar _))  = (show l1) ++ (show l2)
    show (l1@(LVar _) :$: l2)           = (show l1) ++ "(" ++ (show l2) ++ ")"
    show (l1 :$: l2@(LVar _))           = (show l1) ++ (show l2)
    show (l1 :$: l2@I)                  = (show l1) ++ (show l2)
    show (l1 :$: l2@K)                  = (show l1) ++ (show l2)
    show (l1 :$: l2@S)                  = (show l1) ++ (show l2)
    show (l1 :$: l2@B)                  = (show l1) ++ (show l2)
    show (l1 :$: l2@C)                  = (show l1) ++ (show l2)
    show (l1 :$: l2)                    = (show l1) ++ "(" ++ (show l2) ++ ")"
    show (LAbs n l) = "(\\" ++ n ++ " -> " ++ (show l) ++ ")"
    show I = "I"
    show K = "K"
    show S = "S"
    show B = "B"
    show C = "C"

-- ==================================================
--
--  Translator
--
-- ==================================================

translate :: LExp -> LExp

translate (LAbs x e@(LVar y)) | x == y = I
                              | otherwise = K :$: e

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

-- ==================================================
--
--  Testing
--
-- ==================================================

absxy = LAbs "x" (LVar "y")

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
        )
    ]

check :: [(LExp, String)] -> [String]
check l = map f (filter g l)
    where f (e, s) = show (translate e) ++ "  !=  " ++ s
          g (e, s) = show (translate e) /= s

main = aux (check tests)
    where aux [] = putStrLn "Done"
          aux (x:xs) = do putStrLn x; aux xs
