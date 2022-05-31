module Main where

import           Chat.Client (client)

main :: IO ()
main = client "192.168.11.9" 9160
