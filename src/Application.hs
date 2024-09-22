{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}


module Application 
    (app1)
    where

import Data.Text hiding (filter)
import Servant.API
import GHC.Generics
import Servant
import Data.Aeson
import Control.Monad.IO.Class (liftIO)

-- GET /messages/?sortby={approved}
type MessageAPI = "allmessages" :> QueryParam "sortby" SortBy :> Get '[JSON] [Message]
             :<|> "postmessage" :> ReqBody '[JSON] Message
                                :> Post '[JSON] ()


data SortBy = Approved | Author deriving (Eq, Show, Generic)

instance FromHttpApiData SortBy where
  parseUrlPiece sb = case unpack sb of 
    "Approved" -> Right Approved
    "Author" -> Right Author
    _ -> Left . pack $ "unexpexted sortby param"


data Message = Message {
    content :: Text,
    approved :: Bool,
    author :: Text
} deriving (Eq, Show, Generic)

instance FromJSON Message
instance ToJSON Message


messageAPI :: Proxy MessageAPI
messageAPI = Proxy

testMessages :: [Message]
testMessages = [Message (pack "test1") True  (pack "author1"),
                Message (pack "test2") False (pack "author2")]

server1 :: Server MessageAPI
server1 = allmessages
        :<|> postmessage
    where
        allmessages :: Maybe SortBy -> Handler [Message]
        allmessages msortby = return $ case msortby of
          Nothing -> testMessages
          Just Approved  -> filter (\m -> approved m == True) testMessages
          Just Author  -> filter (\m -> author m == (pack "me")) testMessages

        postmessage :: Message -> Handler ()
        postmessage m = do
          liftIO . putStrLn . unpack $ content m 
          return ()

app1 :: Application
app1 = serve messageAPI server1


-- curl -X POST -d '{"content":"Alp Mestanogullari", "approved" : false , "author":"me"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/postmessage
