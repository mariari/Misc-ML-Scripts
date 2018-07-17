
-- modified from http://okmij.org/ftp/Computation/free-monad.html

-- state implemented via free

-- code from my state code
import Control.Arrow
import Control.Monad.Free

newtype State s a = State {runState :: s -> (a, s)}

get :: State s s
get = State (id &&& id)

put :: a -> State a ()
put s = State (const ((), s))

instance Functor (State s) where
  fmap f m = State $ \s -> let (a, s') = runState m s
                           in (f a, s')

type FState s = Free (State s)

getF :: FState s s
getF = liftF get

putF :: s -> FState s ()
putF = liftF . put


runFState :: FState s a -> s -> (a,s)
runFState (Pure x) s = (x,s)
runFState (Free (State f)) s = let (m',s') = f s in runFState m' s'

testState :: FState Int Int
testState = do
  putF 10
  x <- getF
  return x

test_run = runFState testState 0
