{-# LANGUAGE PackageImports #-}
import "WebSockets-example" Application (getApplicationDev, webSocketMain)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort, settingsIntercept)
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
import Network.Wai.Handler.WebSockets (intercept)
import System.IO (putStrLn) 

import Prelude              (IO, ($))
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)
import Yesod.Default.Config
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort, settingsHost, settingsIntercept)
import Network.Wai.Handler.WebSockets (intercept)

-- WebSocket: Changes to bring a reference to App to main, so it can be passed
-- as a parameter to webSocketMain, the WebSocket handler
main :: IO ()
main = do
  config <- load
  (app, foundation) <- getApp config
  runSettings defaultSettings
    { settingsPort = appPort config
    , settingsHost = appHost config
    , settingsIntercept = intercept $ webSocketMain foundation
    } app
  where
    load = (fromArgs parseExtra) 
    getApp = makeApplication

