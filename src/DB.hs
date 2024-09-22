{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module DB 
    (connectToDB) 
    where

import qualified Hasql.Connection as Connection

connectToDB :: IO ()
connectToDB = do
    connectionResult <- Connection.acquire connectionSettings
    case connectionResult of 
        Left (Just errMsg) -> error $ show errMsg
        Left Nothing -> error "Unspecified connection error"
        Right connection -> do
            putStrLn "Acquired connection!"
            Connection.release connection
    where
        -- TODO: импорт конфигурации из файла
        connectionSettings = Connection.settings "localhost" 5432 "postgres" "" "postgres"