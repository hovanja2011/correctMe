module Handlers.Messages 
    (getMessages
    , getSortedByApproveMessages
    , getSortedByAuthorMessages) where

import Types
import Servant
import Data.Profunctor
import qualified Data.Vector as V
import qualified Hasql.TH as TH
import Hasql.Statement
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import Control.Monad.Error.Class (MonadError)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)

-----------------------------------
getMessages :: (MonadDB m, MonadError ServerError m) => m [Message]
getMessages = do
  result <- runSession allMessagesSession
  case result of
    Right messages -> pure messages
    Left err -> parseUsageError err
  where
    parseUsageError msg = throw500 (BS.pack $ show msg)
    throw500 msg = throwError err500 {errBody = LBS.fromStrict msg}

allMessagesSession :: Session [Message]
allMessagesSession = Session.statement () allMessages

allMessages :: Statement () [Message]
allMessages = rmap tuplesToMessages [TH.vectorStatement| select content :: text, approved :: bool, author :: text from "messages"|]
  where
    tupleToMessage (c, ap, au) = Message c ap au
    tuplesToMessages vec = V.toList $ V.map tupleToMessage vec


------------------------------------
getSortedByApproveMessages :: (MonadDB m, MonadError ServerError m) => Bool -> m [Message]
getSortedByApproveMessages as = do
  result <- runSession $ allApproveSortedMessagesSession as
  case result of
    Right messages -> pure messages
    Left err -> parseUsageError err
  where
    parseUsageError msg = throw500 (BS.pack $ show msg)
    throw500 msg = throwError err500 {errBody = LBS.fromStrict msg}

allApproveSortedMessagesSession :: Bool -> Session [Message]
allApproveSortedMessagesSession isA = Session.statement isA allApproveSortedMessages

allApproveSortedMessages :: Statement Bool [Message]
allApproveSortedMessages = rmap tuplesToMessages [TH.vectorStatement| select content :: text, approved :: bool, author :: text from "messages" where approved = $1 :: bool|]
  where
    tupleToMessage (c, ap, au) = Message c ap au
    tuplesToMessages vec = V.toList $ V.map tupleToMessage vec

------------------------------------
getSortedByAuthorMessages :: (MonadDB m, MonadError ServerError m) => Text -> m [Message]
getSortedByAuthorMessages an = do
  result <- runSession $ allAuthorSortedMessagesSession an
  case result of
    Right messages -> pure messages
    Left err -> parseUsageError err
  where
    parseUsageError msg = throw500 (BS.pack $ show msg)
    throw500 msg = throwError err500 {errBody = LBS.fromStrict msg}

allAuthorSortedMessagesSession :: Text -> Session [Message]
allAuthorSortedMessagesSession authorName = Session.statement authorName allAuthorSortedMessages

allAuthorSortedMessages :: Statement Text [Message]
allAuthorSortedMessages = rmap tuplesToMessages [TH.vectorStatement| select content :: text, approved :: bool, author :: text from "messages" where author=$1::text|]
  where
    tupleToMessage (c, ap, au) = Message c ap au
    tuplesToMessages vec = V.toList $ V.map tupleToMessage vec
