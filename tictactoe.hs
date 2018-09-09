import Data.Monoid
import System.Random
import Control.Monad.State
import Control.Monad.Reader

main :: IO ()
main = do
  putStrLn "I'm thinking of a number between 1 and 100, can you guess it?"
  answer  <- getStdRandom (randomR (1,100))
  guesses <- execStateT (guessSession answer) 0
  putStrLn $ "Success in " <> show guesses <> " tries."

guessSession :: Int -> StateT Int IO ()
guessSession ans = modify (+ 1) >> lift getLine >>= cased . flip compare ans . read
  where cased LT = lift (putStrLn "Too low")  >> guessSession ans
        cased GT = lift (putStrLn "Too high") >> guessSession ans
        cased EQ = lift (putStrLn "Got it!")

guessSession' :: Int -> StateT Int IO ()
guessSession' ans = do
  gs <- lift getLine    -- get guess from user
  modify (+1)           -- increment number of guesses
  case compare (read gs) ans of
    LT -> lift (putStrLn "Too low")  >> guessSession ans
    GT -> lift (putStrLn "Too high") >> guessSession ans
    EQ -> lift (putStrLn "Got it!")

test :: ReaderT Int (StateT Int IO) ()
test = ReaderT guessSession'

--fun :: ReaderT Int (StateT Int IO) Integer
fun :: ReaderT Int (StateT Int IO) ()
fun = local (+ 1) $ do
  test
  c <- (+ 100) <$> ask
  b <- get
  liftIO (print c)
  --lift (lift (putStrLn "I'm here"))
  liftIO (putStrLn "I'm here")
  liftIO (print b)
  put b
