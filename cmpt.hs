import Control.Applicative
import Control.Monad (liftM, ap)

-- Types

type State = [(String, Integer)]

data Cmpt a = Cmpt (State -> (a, State))

set :: String -> Integer -> Cmpt ()
set k v = Cmpt(\s -> ((), (k, v) : s))

get :: String -> Cmpt Integer
get k = Cmpt(\s -> (findin s, s))
    where
        findin [] = error ("Unknow key " ++ k)
        findin ((s, i):xs) | k == s = i
                           | otherwise = findin xs

-- Monad

instance Functor Cmpt where -- ask
  fmap = liftM

instance Applicative Cmpt where -- ask
  pure  = return
  (<*>) = ap

instance Monad Cmpt where
    return x = Cmpt (\s -> (x, s))
    m >>= f = Cmpt (\s -> let (x, s') = apply m s in apply (f x) s')
        where apply (Cmpt f) m = f m

-- Main

main = print "cmpt"
