{-# LANGUAGE DeriveGeneric #-}
import qualified Data.Text         as T
import qualified Data.Aeson        as JSON
import           GHC.Generics
import qualified Data.HashTable.IO as H
import           Control.Concurrent.STM
import           Data.Monoid
import           Control.Monad.IO.Class
import           Control.Concurrent.STM
import           Control.Lens
--- TYPES -------------------------------------------------------------------------------------
type Chan   = String
--- DATA STRUCTURES ---------------------------------------------------------------------------

-- IRC State information
data StateConfig = StateConfig
                 { dreamMode   :: [(Chan, Bool)]
                 , muteMode    :: [(Chan, Bool)]
                 } deriving (Show, Generic)


-- Stores the Hash Information per channel
data HashStorage = HashStorage
                 { dream :: Bool
                 , mute  :: Bool
                 }

-- Generates HashStorage
toHashStorage :: Bool -> Bool -> HashStorage
toHashStorage = HashStorage

--  All the Global Variables that make up State
newtype GlobalState = GlobalState
                 {hash :: H.BasicHashTable String HashStorage}

-- Generates GlobalState
toGlobalState :: H.BasicHashTable String HashStorage -> GlobalState
toGlobalState = GlobalState


type Server = String
type Port   = Integer
type Nick   = String
type User   = String
type Host   = String
type Pass   = String

-- IRC network table
data IRCNetwork = IRCNetwork
             { netServer :: Server
             , netPort   :: Port
             , netNick   :: Nick
             , netPass   :: Pass
             , netChans  :: [Chan]
             , netState  :: StateConfig
             } deriving (Show,Generic)


initHash :: [IRCNetwork] -> H.BasicHashTable String HashStorage -> IO ()
initHash net ht = sequence_ $ do
  x  <- net
  y  <- dreamMode . netState $ x
  z  <- muteMode  . netState $ x
  eq <- [(fst y, (snd y, snd z)) | fst y == fst z] -- check if the y and z are talking
  return $ hashadd (netServer x) eq ht           -- about the same channel

hashadd :: String -> (String, (Bool, Bool)) -> H.BasicHashTable String HashStorage -> IO ()
hashadd serv (chan, x) ht = H.insert ht (serv <> chan) (uncurry toHashStorage x)


main = do
  stateT <- H.new >>= newTVarIO . toGlobalState
  state  <- readTVarIO stateT
  initHash nets (hash state)
  return 3
  where nets = [IRCNetwork "lainchan.org" 6697 "loli"      "" ["#lainchan", "#natchan"] (StateConfig [("#lainchan", False), ("#natchan", True)] [("#lainchan", False), ("#natchan", False)]),
                IRCNetwork "freenode.org" 6667 "vomitchan" "" ["#lainchan", "#nootcha"] (StateConfig [("#lainchan", False), ("#nootcha", False)] [("#lainchan", False), ("#nootcha", True)])]
