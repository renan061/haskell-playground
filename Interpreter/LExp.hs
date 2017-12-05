module LExp (LExp(..)) where

data LExp = LVar String
          | LExp :$: LExp
          | LAbs String LExp
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
    show (LAbs n l)                     = "(\\" ++ n ++ " -> " ++ (show l) ++ ")"
    show I = "I"
    show K = "K"
    show S = "S"
    show B = "B"
    show C = "C"
