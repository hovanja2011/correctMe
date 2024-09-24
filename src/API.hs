module API 
    (app)
    where

import           Control.Monad.Reader
import           Servant
import           Types
import           Handlers.Messages
import           Hasql.Pool
import           Data.Text
import           GHC.Int                    (Int64)


type MessageAPI = "messages" :> 
                            ( "all" :> Get '[JSON] [Message]
                             :<|> "sortedby" :> ( 
                                                  "approve" :> QueryParam' '[Required] "isapproved" Bool :> Get '[JSON] [Message]
                                             :<|> "author"  :> QueryParam' '[Required] "authorname" Text :> Get '[JSON] [Message])
                             :<|> "create"                  :> ReqBody '[JSON] Message                   :> Post '[JSON] Int64
                            )
                             
messageAPI :: Proxy MessageAPI
messageAPI = Proxy

app :: Pool -> Application
app pool = serve messageAPI $ hoistServer messageAPI appMToHandler $ server
  where
    appMToHandler :: AppM a -> Handler a
    appMToHandler m = runReaderT (runAppM m) pool

server :: ServerT MessageAPI AppM
server = getAllMessages :<|> (getSortedByApproveMessages :<|> getSortedByAuthorMessages)
                        :<|> createMessage
