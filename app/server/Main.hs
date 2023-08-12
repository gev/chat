module Main where

import           Chat.Server (server)

main :: IO ()
main = server "127.0.0.1" 9160
