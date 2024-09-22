module API 
    (app)
    where

import Control.Monad.Reader
import Servant
import Types
import Handlers.Messages
import Hasql.Pool
import Data.Text

type MessageAPI = "messages" :> 
                            ( "all" :> Get '[JSON] [Message]
                             :<|> "sortbyapprove" :> QueryParam' '[Required] "areapproved" Bool :> Get '[JSON] [Message]
                             :<|> "sortbyauthor" :> QueryParam' '[Required] "authoris" Text :> Get '[JSON] [Message]
                            )
                             
messageAPI :: Proxy MessageAPI
messageAPI = Proxy

app :: Pool -> Application
app pool = serve messageAPI $ hoistServer messageAPI appMToHandler $ server
  where
    appMToHandler :: AppM a -> Handler a
    appMToHandler m = runReaderT (runAppM m) pool

server :: ServerT MessageAPI AppM
server = getMessages :<|> getSortedByApproveMessages :<|> getSortedByAuthorMessages
