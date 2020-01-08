import Data.Monoid
import Control.Applicative

data Reader r a = Reader {runReader :: r -> a}

ask = Reader id

greeter :: Reader String String
greeter = ask >>= pure . ("hello, " <>) . (<> "!")


instance Functor (Reader r) where
  fmap f (Reader m) = Reader $ f . m

-- f <*> g = \x -> f x (g x)
instance Applicative (Reader r) where
  pure                  = Reader . const
  Reader f <*> Reader a = Reader (f <*> a)
-- alternative definition
--  m <*> k = Reader $ \r -> let f = runReader m r
--                               a = runReader k r
--                            in f a

-- f =<< g = \x -> f (g x) x
instance Monad (Reader r) where
  Reader m >>= k = Reader (runReader . k =<< m)
  --m >>= k = Reader $ \r -> runReader (k (runReader m r)) r
  --m >>= k = Reader $ runReader . k  =<< runReader m

hello :: Reader String String
hello = do
  name <- ask
  return $ "hello, " <> name <> "!"

bye :: Reader String String
bye = do
  name <- ask
  pure $ "bye, " <> name <> "!"

convo :: Reader String String
convo = do
  c1 <- hello
  c2 <- bye
  pure $ c1 <> " " <> c2

convo' :: Reader String String
convo' = (<>) <$> hello <*> ((" " <>) <$> bye)

convo'' :: Reader String String
convo'' = liftA2 (<>) hello ((" " <>) <$> bye)
