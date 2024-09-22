{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}


module Application 
    (app1)
    where

import Data.Text hiding (filter)
import Servant.API
import GHC.Generics
import Servant
import Data.Aeson

-- GET /messages/?sortby={approved}
type MessageAPI = "messages" :> QueryParam "sortby" SortBy :> Get '[JSON] [Message]

data SortBy = Approved | Author deriving (Eq, Show, Generic)

instance FromHttpApiData SortBy where
  parseUrlPiece sb = case unpack sb of 
    "Approved" -> Right Approved
    "Author" -> Right Author
    _ -> Left . pack $ "unexpexted sortby param"


data Message = Message {
    content :: String,
    approved :: Bool,
    author :: String
} deriving (Eq, Show, Generic)

instance ToJSON Message

messageAPI :: Proxy MessageAPI
messageAPI = Proxy

testMessages :: [Message]
testMessages = [ Message "test1" True "author1",
                 Message "test2" False "author2"]

server1 :: Server MessageAPI
server1 = messages
    where
        messages :: Maybe SortBy -> Handler [Message]
        messages msortby = return $ case msortby of
          Nothing -> testMessages
          Just Approved  -> filter (\m -> approved m == True) testMessages
          Just Author  -> filter (\m -> author m == "me") testMessages

app1 :: Application
app1 = serve messageAPI server1