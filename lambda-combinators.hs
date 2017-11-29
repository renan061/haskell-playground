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
--  Transformer
--
-- ==================================================

transform :: LExp -> LExp

transform (LAbs x e@(LVar y)) | x == y = I
                              | otherwise = K :$: e

transform (LAbs x (e1 :$: e2)) = s (transform abs1) (transform abs2)
    where s c1 c2 = (S :$: c1) :$: c2
          abs1 = LAbs x e1
          abs2 = LAbs x e2

transform (LAbs x l@(LAbs y e)) | x == y    = K :$: (transform l)
                                | otherwise = transform (LAbs x (transform l))

transform (LAbs x e) = K :$: e

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
            "S(KK)I"
        ),
        (
            LAbs "x" (LAbs "y" (LVar "y")),
            "KI"
        ),
        (
            LAbs "x" ((LVar "x") :$: (LVar "y")),
            "SI(Ky)"
        ),
        -- Other tests
        (
            -- \y -> f x y
            LAbs "y" ((LVar "f") :$: (LVar "x") :$: (LVar "y")),
            "S(S(Kf)(Kx))I"
        ),
        (
            -- \x -> plus 1 x
            LAbs "x" ((LVar "plus") :$: (LVar "1") :$: (LVar "x")),
            "S(S(K plus )(K1))I"
        ),
        (
            -- \x -> \y -> x y
            LAbs "x" (LAbs "y" ((LVar "x") :$: (LVar "y"))),
            "S(S(KS)(S(KK)I))(KI)"

        )
    ]

check :: [(LExp, String)] -> [String]
check l = map f (filter g l)
    where f (e, s) = show (transform e) ++ "  !=  " ++ s
          g (e, s) = show (transform e) /= s

main = aux (check tests)
    where aux [] = putStrLn "Done"
          aux (x:xs) = do putStrLn x; aux xs
