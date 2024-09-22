{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main (main) where

import API
import DB
import Network.Wai.Handler.Warp


main :: IO ()
main =  do
    connectToDB
    run 8081 app
