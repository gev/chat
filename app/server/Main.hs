module Main where

import           Chat.Server (server)

main :: IO ()
main = server "0.0.0.0" 9160
