module LExp (LExp(..)) where

data LExp = LVar String
          | LExp :$: LExp
          | LAbs String LExp
          | I
          | K
          | B
          | C

instance Show LExp where
    show (LVar n) | length n <= 1 = n
                  | otherwise     = "." ++ n ++ "."
    show (l1 :$: l2) = "(" ++ (show l1) ++ " " ++ (show l2) ++ ")"
    show (LAbs n l)  = "(\\" ++ n ++ " -> " ++ (show l) ++ ")"
    show I = "I"
    show K = "K"
    show B = "B"
    show C = "C"
