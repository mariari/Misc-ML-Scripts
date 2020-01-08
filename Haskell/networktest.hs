{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC (putStrLn)
import           Network
import           Data.Text.Encoding
import           Data.Foldable
import qualified Network.Connection    as C
import           System.IO
import           Data.Monoid

main :: IO ()
main = do
    ctx <- C.initConnectionContext
    con <- C.connectTo ctx C.ConnectionParams { C.connectionHostname  = "irc.freenode.net"
                                             , C.connectionPort      = 8000
                                             , C.connectionUseSecure = Nothing -- Just $ C.TLSSettingsSimple False False True
                                             , C.connectionUseSocks  = Nothing
                                             }
    writeSSL con ("NICK", "vomss")
    writeSSL con ("USER", "voms" <> " 0 * :connected")
    --unless ("stuff" == "") (writeSSL con ("NICKSERV :IDENTIFY", "stuff") >> waitForAuth con)
    mapM_ (writeSSL con) (zip (repeat "JOIN") ["#lainchan","##loli"])
    return ()
  where
    waitForAuth h = C.connectionGetLine 1000 h >>= \line -> BC.putStrLn line
                                  >> unless (":You are now identified" `BS.isInfixOf` line) (waitForAuth h)
  



--writeSSL :: C.Connection -> (T.Text, T.Text) -> IO ()
writeSSL :: C.Connection -> (BS.ByteString, BS.ByteString) -> IO ()
writeSSL h (act,args) = C.connectionPut h (fold [act, " ", args, "\r\n"])
