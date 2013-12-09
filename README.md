Yesod Scaffold WebSockets example
=================================

This is an example using WebSockets in a scaffolded Yesod site. You can check the commit log for a diff of the things that have been changed to make the scaffold support WebSockets. It requires WebSockets 0.8 and wai-websockets 1.3.2.

Random Notes:

* Unfortunately, to make WebSockets work, you have to change a bunch of things in main.hs and devel.hs.

* All of the comments for the new code are tagged with "WebSocket"

* This code adds an MVar to App to track connections by UserId, as the most common case would seem to be authenticated. If you don't need to track user ids, this could instead be an `MVar [WS.Connection]`.

* The actual WebSocket handlers are in AppWebSocket.hs.

* WebSocket handlers occur in the `Socket` monad, which is just a `ReaderT SocketData (LoggingT IO)` monad, so that you can use the Logging macros (e.g. `$(logDebug)` and for convenience.

* A function called `liftHandler` is provided to run `HandlerT` actions inside `Socket`, which is based on `runFakeHandler`. I'm not sure of the overhead of this. Also, since it's provided with a fake request that only has its headers set, it will fail on some actions. It's probably best to only use it for simple actions like database stuff.

* You *must* run yesod with 'yesod devel -n' to disable the yesod devel reverse proxy. WebSockets don't work through the reverse proxy.

* To see this in action, start the server with 'yesod devel -n' and browse to localhost:3000. You will have to login with the dummy login (just enter any name). On the home page there is an example box that simply echoes back via WebSockets anything you enter to the page.

* This is not tested in production.

* This isn't a tight integration with Yesod, so you can expect not to have the same level of niceties, like type-safe Routing.
