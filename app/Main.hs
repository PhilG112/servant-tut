module Main where

import Server (app1, app2, app3)
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8081 app3
