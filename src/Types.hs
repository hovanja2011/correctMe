
module Types where


import Data.Text                  hiding (filter)
import Servant.API
import GHC.Generics
import Data.Aeson
import           GHC.Int                    (Int64)

import Control.Monad.Reader
import Servant
import Hasql.Session              (Session)
import Control.Monad.Error.Class  (MonadError)
import Hasql.Pool
import GHC.Encoding.UTF8

---------------------- Message ---------------------

data Message = Message {
    id :: Int64,
    content :: Text,
    author :: Text,
    approved :: Bool
} deriving (Eq, Show, Generic)

instance FromJSON Message
instance ToJSON Message


---------------------- IdentM ---------------------

data IdentM = IdentM {
    identM :: Int64
} deriving (Eq, Show, Generic)

instance FromJSON IdentM
instance ToJSON IdentM


instance FromHttpApiData IdentM where
  parseUrlPiece s = Right $ IdentM ((read $ unpack s ) :: Int64)
    -- _       -> Left . pack $ "unexpexted sortby param"


---------------------- SortBy ---------------------

data Approved = Approved {
    isapproved :: Bool
}

instance FromHttpApiData Approved where
  parseUrlPiece s = case s of
    "true"  -> Right $ Approved True
    "false" -> Right $ Approved False
    _       -> Left . pack $ "unexpexted sortby param"


data Author = Author {
    name :: Text
}

-- data SortBy = Approved | Author deriving (Eq, Show, Generic)

-- instance FromHttpApiData SortBy where
--   parseUrlPiece sb = case unpack sb of 
--     "Approved" -> Right Approved
--     "Author" -> Right Author
--     _ -> Left . pack $ "unexpexted sortby param"


---------------------- AppM ---------------------

class MonadIO m =>
      MonadDB m
  where
  runSession :: Session a -> m (Either UsageError a)

newtype AppM a =
  AppM
    { runAppM :: ReaderT (Pool) (Handler) a
    }
  deriving (Functor, Applicative, Monad, MonadIO, Generic, MonadError ServerError)

instance MonadDB AppM where
  runSession sess =
    AppM $ do
      pool <- ask
      result <- liftIO $ use pool sess
      runAppM $ pure result

