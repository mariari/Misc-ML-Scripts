import Text.Parsec
--import Control.Monad.State
import Data.Text
import Network.TLS
import Control.Monad.Trans.State
import qualified Data.HashTable.IO as H
import Control.Monad.IO.Class
import Control.Concurrent.STM as S
--integerParser :: Parsec Text () Integer
--integerParser = read <$> many1 digit
-- ht <- H.new :: IO (H.BasicHashTable String String)

funTest :: ((), Integer)
funTest = flip runState 1 $ do n <- get
                               put(n+1)

funTest2 :: ((), Integer)
funTest2 = flip runState 1 $ get >>= put . (+1)
funTest5 = flip runState 1 $ state (\x -> ((), x + 1))


funTest3 = runState (state (\_ -> ((), 2)) >> state (\s -> (s,s))) 3
funTest4 = runState (put 2 >> get) 3

funTest6 = runState (get >>= put . (+ 2) >> state (\x -> (x,x + 3))) 3

funTest7 = evalState (get >>= put . (+3) >> return 4) 2

funTest8 = do
  ht <- (H.new :: IO (H.BasicHashTable String String))
  runStateT (get >>= \x -> liftIO (H.insert x "bad" "not")) ht

funTest8'' :: IO ((),(H.BasicHashTable String String))
funTest8'' = H.new >>= runStateT (get >>= \x -> liftIO (H.insert x "bad" "not"))

funTest8' = do
  ht <- (H.new :: IO (H.BasicHashTable String String))
  runStateT (get >>= liftIO . (\x -> H.insert x "bad" "not")) ht

funTest9 :: (Integer, Integer)
funTest9 = runState (put 2 >> return (2 + 3)) 2

stateModify :: (Num a, Monad m) => StateT a m ()
stateModify = get >>= put . (+3) >> get >>= put . (+3)

stateIncreaseBy n = modify (+ n)

funTest10 = runState (return 2 >>= stateIncreaseBy) 3

h = (H.new :: IO (H.BasicHashTable String String))

funTest19 = do
  ht   <- H.new :: IO (H.BasicHashTable String String)
  let blah = ht
  z    <- newTVarIO ht
  hash <- readTVarIO z
  (H.insert hash "stuff" "bad")
--  modifyTVar z (H.insert "stuff" "bad")


data Test = Test {f :: Int -> Int}
