{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client as C
import Control.Monad.Catch
import Network.HTTP.Simple
import Data.ByteString.Lazy as L
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Search as S
import Control.Monad

x ::  MonadThrow m  => m Request
x = (C.parseRequest "https://lainchan.org")


getInfo :: (MonadIO m, MonadThrow m) => m (Response L.ByteString)
getInfo = httpLBS =<< x

getHeader :: (MonadThrow f, MonadIO f) => String -> f ByteString
getHeader = (L.drop (L.length "<title>") . fst . S.breakOn "</title>" . snd . S.breakOn "<title>" . responseBody <$>) . httpLBS <=< C.parseRequest

-- y <- responseBody <$> getInfo

-- let (x,z) = S.breakOn "<title>" y
-- let (i,_) = (S.breakOn "</title>" z)
-- L.drop (L.length "<title>") i