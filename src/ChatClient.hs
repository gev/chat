{-# LANGUAGE OverloadedStrings #-}

module ChatClient (client) where

import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Network.Socket      (withSocketsDo)
import qualified Network.WebSockets  as WS

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    forkIO $
        forever $ do
            msg <- WS.receiveData conn
            liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)

client :: IO ()
client = WS.runClient "192.168.11.9" 9160 "/" app
