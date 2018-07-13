import Control.Arrow

newtype State s a = State {runState :: s -> (a, s)}

get :: State s s
get = State (id &&& id)

put :: a -> State a ()
put s = State (const ((), s))

modify :: (s -> s) -> State s ()
modify f = State (const () &&& f)

(>>=%) :: State s t -> (t -> State s a) -> State s a
m >>=% k = State $ \s -> let (a, s') = runState m s
                         in runState (k a) s'

return' :: a -> State s a
return' a = State (\s -> (a,s))

g x = get >>= put . (x +)

instance Functor (State s) where
  fmap f m = State $ \s -> let (a, s') = runState m s
                           in (f a, s')

instance Applicative (State s) where
  pure    = return'
  k <*> m = State $ \s -> let (f, s')  = runState k s
                              (a, s'') = runState m s'
                           in (f a, s'') -- can use *** here, but it is less readable

instance Monad (State s) where
  return = return'
  (>>=)  = (>>=%)


funtest :: (Integer, Integer)
funtest = flip runState 3 $ do
  modify (+ 1)
  x <- get
  g x
  modify (+ 99)
  pure 5

-- (5,107)



-- (+ 1) <$> (+ 2) <$> Just 3
