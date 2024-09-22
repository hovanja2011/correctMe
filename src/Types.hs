
module Types where

import Data.Text hiding (filter)
import Servant.API
import GHC.Generics
import Data.Aeson


--  TODO: выделить отдельный модуль с типами
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