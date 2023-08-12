{-# LANGUAGE OverloadedStrings #-}

module Chat.Server (server) where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar,
                                     readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forM_, forever)
import           Data.Char
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T
import qualified Network.WebSockets as WS

-- We represent a client by their username and a `WS.Connection`. We will see how we
-- obtain this `WS.Connection` later on.
type Client = (Text, WS.Connection)

-- The state kept on the server is simply a list of connected clients. We've added
-- an alias and some utility functions, so it will be easier to extend this state
-- later on.
type ServerState = [Client]

-- Create a new, initial state:
newServerState :: ServerState
newServerState = []

-- Check if a user already exists (based on username):
clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

-- Add a client (this does not check if the client already exists, you should do
-- this yourself using `clientExists`):
addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

-- Remove a client:
removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

-- Send a message to all clients, and log it on stdout:
broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

-- The talk function continues to read messages from a single client until he
-- disconnects. All messages are broadcasted to the other clients.
talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
  msg <- WS.receiveData conn
  broadcast (user `mappend` ": " `mappend` msg) =<< readMVar state

-- Our server application has the type:
application :: MVar ServerState -> WS.ServerApp
-- Note that `WS.ServerApp` is nothing but a type synonym for
-- `WS.PendingConnection -> IO ()`.

-- Our application starts by accepting the connection. In a more realistic
-- application, you probably want to check the path and headers provided by the
-- pending request.

-- We also fork a pinging thread in the background. This will ensure the connection
-- stays alive on some browsers.
application state pending =
  do
    conn <- WS.acceptRequest pending
    let req = WS.pendingRequest pending
    let name = T.tail $ T.decodeUtf8 $ WS.requestPath req
    if any ($ name) [T.null, T.any isPunctuation, T.any isSpace]
      then
        WS.sendTextData conn
              ( "Name cannot "
                  <> "contain punctuation or whitespace, and "
                  <> "cannot be empty" :: Text
              )
      else (do
        let client = (name, conn)
        clients <- readMVar state
        if clientExists client clients
          then WS.sendTextData conn ("User already exists" :: Text)
          else (do
            modifyMVar_ state $ \s -> do
              let s' = addClient client s
              WS.sendTextData conn $
                "Welcome! Users: "
                  <> T.intercalate ", " (map fst s)
              broadcast (name <> " joined") s'
              return s'
            WS.withPingThread conn 30 (return ()) $ do
              finally
                (talk client state)
                (do
                  s <- modifyMVar state $ \s ->
                    let s' = removeClient client s in return (s', s')
                  broadcast (fst client <> " disconnected") s
                )
            )

      )

server :: String -> Int -> IO ()
server host port = do
  state <- newMVar newServerState
  WS.runServer host port $ application state
