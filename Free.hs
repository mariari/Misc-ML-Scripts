{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Free
import Data.Monoid
import Text.Show.Functions
import System.Exit hiding (ExitSuccess)
import Test.QuickCheck
import Data.List
import Control.Monad


data DBTransaction' x = Query String ([String] -> x)
                      | Write String String x
                      deriving (Functor)

type DBTransaction = Free DBTransaction'

query :: String -> DBTransaction [String]
query str = liftF (Query str id)

write :: String -> String -> DBTransaction String
write loc txt = liftF (Write loc txt txt)

--transaction :: [Char] -> DBTransaction [[[[String]]]]
transaction :: String -> Free DBTransaction' ()
transaction str = do
  databases <- query "show * in Database"
  let dbLeft = filter (str `isInfixOf`) databases
  forM_ dbLeft $ \db -> do
    tables <- query ("show * in " <> db)
    forM_ tables $ \table -> do
      schemas <- query ("show * in table " <> table <> " in database " <> db)
      forM_ schemas $ \schema -> do
        creation <- query ("show cr in schema " <> schema <> " in table " <> table <> " in database " <> db)
        forM_ creation (write (db <> "/" <> schema))

runPure :: Free DBTransaction' a -> [[String]] -> [(String, [String])]
runPure = go
  where go (Pure a)               _        = []
        go (Free (Query qr next)) (x:xs)   = (qr,x) : go (next x) xs
        go (Free (Query qr next)) []       = []
        go (Free (Write file txt next)) xs = ("wrote file " <> file <> " with text " <> txt,[txt]) : go next xs

inputs = [["notHeart", "testHeart", "MoreTest", "testA"] -- dbs
         ,["table1", "tabl2"]          -- tables in testHeart
         ,["schema1", "more call"]     -- sechemas in table1
         ,["text", "text2"]            -- text in schema1
         ,["text3", "text4"]           -- text in more call
         ,["schema2", "schema3"]       -- sechemas in table2
         ]

-- runPure (transaction "test") inputs
-- output of above!
response = [("show * in Database",["notHeart","testHeart","MoreTest","testA"])
           ,("show * in testHeart",["table1","tabl2"])
           ,("show * in table table1 in database testHeart",["schema1","more call"])
           ,("show cr in schema schema1 in table table1 in database testHeart",["text","text2"])
           ,("wrote file testHeart/schema1 with text text",["text"])
           ,("wrote file testHeart/schema1 with text text2",["text2"])
           ,("show cr in schema more call in table table1 in database testHeart",["text3","text4"])
           ,("wrote file testHeart/more call with text text3",["text3"])
           ,("wrote file testHeart/more call with text text4",["text4"])
           ,("show * in table tabl2 in database testHeart",["schema2","schema3"])]

-- Gotten from Free  From Tree & Halogen VDOM--------------------------------------------
data Talker = Speak String Talker
            | Listen (String -> Talker)
            | Done
            deriving Show

{-program = do
  Speak "Hello"
  Speak "What is your name?"
  name <- Listen
  Speak ("Nice to meet you, " <> name)
-}

program' :: Talker
program' = Speak "Hello"
                 (Speak "What is your name?"
                         (Listen (\name ->
                                    (Speak ("Nice to meet you, " <> name)
                                           Done))))

runProgram :: String -> IO ()
runProgram name = go program'
  where
    go Done             = pure ()
    go (Listen reply)   = go (reply name)
    go (Speak str next) = do
      print str
      go next

-- Time to abstract with free


data TalkerF x
  = SpeakF String x
  | ListenF (String -> x)
  deriving (Show, Functor)

-- this is also valid!
-- data TalkerF a = Free Talker a
type TalkerM = Free TalkerF

speak :: String -> TalkerM ()
speak str = liftF (SpeakF str ())

listen :: TalkerM String
listen = liftF (ListenF id)

program = do
  speak "Hello"
  speak "What is your name?"
  name <- listen
  speak ("Nice to meet you, " <> name)


runProgramImp :: TalkerM a -> IO a
runProgramImp = go
  where
    go (Pure a)                 = pure a
    go (Free (ListenF reply))   = do
      name <- getLine
      go (reply name)
    go (Free (SpeakF str next)) = do
      print str
      go next

runProgramPure :: TalkerM a -> [String] -> [String]
runProgramPure = go
  where
    go (Pure a)                 _       = []
    go (Free (ListenF reply))   (x:xs)  = go (reply x) xs
    go (Free (ListenF reply))   []      = []
    go (Free (SpeakF str next)) xs      = str : go next xs
