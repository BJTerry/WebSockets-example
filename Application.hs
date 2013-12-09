{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    , webSocketMain -- WebSocket: Export WebSocket handler
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.HTTP.Conduit (newManager, def)
import Control.Monad.Logger (runLoggingT)
import System.IO (stdout)
import System.Log.FastLogger (mkLogger)

-- WebSocket: New imports
import AppWebSocket (runSocket, webSocketHandler)
import qualified Network.WebSockets as WS
import Control.Concurrent.MVar
import System.Environment (getEnvironment)
import Data.Maybe (fromMaybe)
import Safe (readMay)
import qualified Data.Map.Strict as Map


-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, App)
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = Logger $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    -- WebSocket: Need to return App
    return $ (logWare app, foundation)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)
    logger <- mkLogger True stdout
    sessionBackend <- makeAppSessionBackend    
    -- WebSocket: This mvar will hold a Map of the active WebSocket connections
    mvar <- newMVar Map.empty
    let foundation = App conf s p manager dbconf logger sessionBackend mvar

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation
    

-- for yesod devel
-- WebSocket: Changed to return Appl so it's available in main
getApplicationDev :: IO (Int, Application, App)
getApplicationDev =
    wsDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

-- WebSocket: To thread App into main, we have to change defaultDevelApp
wsDevelApp
    :: (Show env, Read env)
    => IO (AppConfig env extra) -- ^ A means to load your development @'AppConfig'@
    -> (AppConfig env extra -> IO (Application, App)) -- ^ Get your @Application@
    -> IO (Int, Application, App)
wsDevelApp load getApp = do
    conf   <- load
    env <- getEnvironment
    let p = fromMaybe (appPort conf) $ lookup "PORT" env >>= readMay
        pdisplay = fromMaybe p $ lookup "DISPLAY_PORT" env >>= readMay
    putStrLn $ "Devel application launched: http://localhost:" ++ show pdisplay
    (app, foundation) <- getApp conf
    return (p, app, foundation)


-- | WebSocket: The main ServerApp for WebSocket connections
webSocketMain :: App -> WS.ServerApp
webSocketMain app pc = runSocket app pc webSocketHandler


