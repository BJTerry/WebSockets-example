{-# LANGUAGE PackageImports #-}
import "WebSockets-example" Application (getApplicationDev, webSocketMain)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort, settingsIntercept)
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)

import Network.Wai.Handler.WebSockets (intercept)

-- WebSocket: Changes to bring a reference to App to main, so it can be passed
-- as a parameter to webSocketMain, the WebSocket handler
main :: IO ()
main = do
    putStrLn "Starting devel application"
    (port, app, foundation) <- getApplicationDev
    forkIO $ runSettings defaultSettings
        { settingsPort = port
        , settingsIntercept = intercept $ webSocketMain foundation
        } app
    loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "yesod-devel/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
