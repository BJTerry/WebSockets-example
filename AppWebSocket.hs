module AppWebSocket where

import Import
import Yesod.Auth
import qualified Network.WebSockets as WS
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.Logger (runLoggingT, LoggingT)
import qualified Data.Map.Strict as Map
import Network.Wai (Request(..))
import Control.Exception (finally)
import Control.Monad (forever)
import Data.Text (pack, concat)
import Control.Concurrent.MVar


-- | WebSocket: The main WebSocket handler
webSocketHandler :: Socket ()
webSocketHandler = do
  path <- fmap WS.requestPath getRequestHead
  $(logDebug) $ pack $ show path
  case path of -- Different handlers depending on the path of the request
    "/" -> defaultHandler
    -- Put more paths and handlers heare
    _ -> defaultHandler
  return ()

-- | WebSocket: An example handler which authenticates the user and keeps track of connections by UserId
--   This handler just echos information received back to the user.
defaultHandler :: Socket ()
defaultHandler = do
  conn <- getConn -- Gets the WS.Connection
  Right (Entity uid user) <- liftHandler requireAuth -- Runs requireAuth in the HandlerT monad
  runSock <- socketToIO -- Grab the action which allows socket to be run in the IO monad
  liftIO $ finally (runSock $ do -- Run the following in the Socket monad 
    connectUser conn uid -- Add the user to the appConnections Map
    forever $ do
      d <- wsReceiveData :: Socket Text -- Receieve data from the user
      $(logDebug) $ Data.Text.concat ["Received WS message: ", d] -- Socket allows us to use logging macros
      wsSendText d -- Echos the received data back to the We]bSocket
    ) (runSock $ handleDisconnect uid) -- Always disconnect and remove user from the appConnections Map

-- | WebSocket: Adds a new user to the appConnections Map
connectUser :: WS.Connection -> UserId -> Socket ()
connectUser conn uid = do
  $(logDebug) $ pack $  "User connect: " ++ show uid  
  app <- getApp
  liftIO $ modifyMVar (appConnections app) $ \conns ->
    return (Map.insert uid conn conns, ())

-- | WebSocket: Removes a user from the appConnections Map
handleDisconnect :: UserId -> Socket ()
handleDisconnect uid = do
  $(logDebug) $ pack $  "User disconnect: " ++ show uid
  app <- getApp      
  liftIO $ modifyMVar (appConnections app) $ \conns ->
    return (Map.delete uid conns, ())

-- | WebSocket: SocketData is data available within the Socket monad
data SocketData = SocketData 
  { wsApp :: App
  , wsSessMap :: SessionMap
  , wsConn :: WS.Connection
  , wsRequestHead :: WS.RequestHead
  }

-- | WebSocket: A monad for running WebSocket actions. It includes Logging and a reader
--   for the SocketData type.
newtype Socket a = Socket 
  { unSocket :: ReaderT SocketData (LoggingT IO) a
  } deriving (Functor, Monad, MonadIO, MonadReader SocketData, MonadLogger)

-- WebSocket: Run actions in the WebSocket monad
runSocket :: App -> WS.PendingConnection -> Socket a -> IO a
runSocket app pc f = do
  sMap <- loadWSSessionMap pc app  
  conn <- WS.acceptRequest pc
  let reqHead = WS.pendingRequest pc
  let sd = SocketData app sMap conn reqHead
  runLoggingT (runReaderT (unSocket f) sd) (messageLoggerSource app (appLogger app))

-- WebSocket: Convenience functions for the Socket monad

-- WebSocket: Get the App
getApp :: Socket App
getApp = fmap wsApp ask

-- WebSocket: Get the current WebSocket connection
getConn :: Socket WS.Connection
getConn = fmap wsConn ask

-- WebSocket: Get the request headers
getRequestHead :: Socket WS.RequestHead
getRequestHead = fmap wsRequestHead ask

-- WebSocket: Send text to the WebSocket
wsSendText :: WS.WebSocketsData a => a -> Socket ()
wsSendText d = do
  conn <- getConn
  liftIO $ WS.sendTextData conn d

-- WebSocket: Send binary data to the WebSocket
wsSendBinary :: WS.WebSocketsData a => a -> Socket ()
wsSendBinary d = do
  conn <- getConn
  liftIO $ WS.sendBinaryData conn d

-- WebSocket: Receive data from the WebSocket
wsReceiveData :: WS.WebSocketsData a => Socket a
wsReceiveData = do
  conn <- getConn
  liftIO $ WS.receiveData conn

-- WebSocket: Run socket actions in the IO monad, if needed within catch statements or for new threads
socketToIO :: Socket (Socket a -> IO a)
socketToIO = do
  sd <- ask
  let app = wsApp sd
  return (\f -> runLoggingT (runReaderT (unSocket f) sd) (messageLoggerSource app (appLogger app)))

-- WebSocket: Left HandlerT actions to the Socket monad
liftHandler :: HandlerT App IO a -> Socket (Either ErrorResponse a)
liftHandler f = do
  sd <- ask
  runFakeHandler (wsSessMap sd) appLogger (wsApp sd) f

-- WebSocket: Load a sessionMap from the WebSocket pending connection using the SessionBackend
loadWSSessionMap :: WS.PendingConnection -> App -> IO SessionMap
loadWSSessionMap pc app = do
  let req = Network.Wai.Request {requestHeaders = WS.requestHeaders $ WS.pendingRequest pc}
  (sessMap, _) <- sbLoadSession (appSessionBackend app) $ req
  return sessMap


