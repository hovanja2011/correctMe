{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}


module API 
    (app)
    where

import Control.Monad.Reader
import Data.Text hiding (filter)
import Servant
import Types



messageAPI :: Proxy MessageAPI
messageAPI = Proxy

testMessages :: [Message]
testMessages = [Message (pack "test1") True  (pack "author1"),
                Message (pack "test2") False (pack "author2")]

app :: Application
app = serve messageAPI server

server :: Server MessageAPI
server = allmessages
        :<|> postmessage
    where
        allmessages :: Maybe SortBy -> Handler [Message]
        allmessages msortby = return $ case msortby of
          Nothing -> testMessages
          Just Approved  -> Prelude.filter (\m -> approved m == True) testMessages
          Just Author  -> Prelude.filter (\m -> author m == (pack "me")) testMessages

        postmessage :: Message -> Handler ()
        postmessage m = do
          liftIO . putStrLn . unpack $ content m 
          return ()

-- curl -X POST -d '{"content":"Alp Mestanogullari", "approved" : false , "author":"me"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/postmessage
