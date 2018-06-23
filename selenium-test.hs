{-# LANGUAGE OverloadedStrings #-}
import Test.WebDriver as W
import Data.Aeson
import Data.HashMap.Lazy
import Control.Concurrent

ourChrome :: Browser
ourChrome = Chrome Nothing (Just "/usr/lib/chromium/chromium") [] [] (fromList [])

firefoxConfig :: WDConfig
firefoxConfig = useBrowser ourChrome defaultConfig

main :: IO ()
main = runSession firefoxConfig $ do
  openPage "http://google.com"
  searchInput <- findElem (ByCSS "input[type='text']")
  sendKeys "Hello, World!" searchInput
  submit searchInput
  _ <- return $ threadDelay 1
  closeSession
