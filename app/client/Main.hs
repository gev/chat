module Main where

import           Chat.Client (client)

main :: IO ()
main = client "127.0.0.1" 9160
