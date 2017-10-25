-- lambda expression
data Lambda = LVar Name
            | LApp Lambda Lambda
            | LAbs Name Lambda
    deriving Show

type Name = String

-- free variable
fv :: Lambda -> [Name]
fv (LVar x) = [x]
fv (LApp m n) = fv m ++ fv n
fv (LAbs x m) = filter (/= x) (fv m)

-- new letter
nl :: Lambda -> String
nl (LVar x) = x ++ x
nl (LApp m n) = nl m ++ nl n
nl (LAbs x m) = x ++ nl m

-- substitution
subst :: Lambda -> Name -> Lambda -> Lambda

subst m@(LVar y) x n | x == y = n
                     | otherwise = m

subst (LApp m1 m2) x n = LApp (subst m1 x n) (subst m2 x n)

subst (LAbs y m) x n | x == y = LAbs x m
                     | y `notin` (fv n) = LAbs y (subst m x n)
                     | otherwise = LAbs new (subst (subst m y (LVar new)) x n)
    where new = x ++ y ++ nl m ++ nl n -- todo
          notin x l = find (== x) l
          find p [] = False
          find p (x:xs) | p x = True
                        | otherwise = find p xs

-- testing

_x = LVar "x"                           -- x
_y = LVar "y"                           -- y
_app = LApp _x _y                       -- x y
_abs = LAbs "x" _app                    -- \x. x y
_t1 = LAbs "x" (LAbs "y" (LVar "a"))    -- \x. \y. a

-- main = print (fv _t1)
-- main = print _x
main = print (subst _t1 "a" _y) -- (\x. \y. a)[a -> y]
