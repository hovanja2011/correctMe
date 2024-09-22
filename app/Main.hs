{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main (main) where

import API
import Network.Wai.Handler.Warp
import Hasql.Pool (acquire)

main :: IO ()
main =  do
  pool <- acquire 1 1 1 1 settings
  run 8080 $ app pool
  where
    settings = "postgresql://postgres:postgres@localhost:5432/postgres"

