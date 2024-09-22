module Main (main) where

import Application
import Network.Wai.Handler.Warp



main :: IO ()
main = run 8081 app1
