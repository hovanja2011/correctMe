module Handlers.Messages 
    ( getAllMessages
    , getSortedByApproveMessages
    , getSortedByAuthorMessages
    , createMessage) where

import           Types
import           Utils
import           Servant
import           Data.Profunctor
import qualified Data.Vector                as V
import qualified Hasql.TH                   as TH
import           Hasql.Statement
import           Hasql.Session              (Session)
import qualified Hasql.Session              as Session
import           Data.Text                  (Text)
import           GHC.Int                    (Int64)
import           Control.Monad.Error.Class  (MonadError)



-----------------getAllMessages-----------------
getAllMessages :: (MonadDB m, MonadError ServerError m) => m [Message]
getAllMessages = do
  result <- runSession allMessagesSession
  case result of
    Right messages -> pure messages
    Left err -> parseUsageError err

allMessagesSession :: Session [Message]
allMessagesSession = Session.statement () allMessages

allMessages :: Statement () [Message]
allMessages = rmap tuplesToMessages [TH.vectorStatement| select content :: text, approved :: bool, author :: text from "messages"|]
  where
    tupleToMessage (c, ap, au) = Message c ap au
    tuplesToMessages vec = V.toList $ V.map tupleToMessage vec


-----------------getSortedByApproveMessages-----------------
getSortedByApproveMessages :: (MonadDB m, MonadError ServerError m) => Bool -> m [Message]
getSortedByApproveMessages as = do
  result <- runSession $ allApproveSortedMessagesSession as
  case result of
    Right messages -> pure messages
    Left err -> parseUsageError err

allApproveSortedMessagesSession :: Bool -> Session [Message]
allApproveSortedMessagesSession isA = Session.statement isA allApproveSortedMessages

allApproveSortedMessages :: Statement Bool [Message]
allApproveSortedMessages = rmap tuplesToMessages [TH.vectorStatement| select content :: text, approved :: bool, author :: text from "messages" where approved = $1 :: bool|]
  where
    tupleToMessage (c, ap, au) = Message c ap au
    tuplesToMessages vec = V.toList $ V.map tupleToMessage vec

-----------------getSortedByAuthorMessages-----------------
getSortedByAuthorMessages :: (MonadDB m, MonadError ServerError m) => Text -> m [Message]
getSortedByAuthorMessages an = do
  result <- runSession $ allAuthorSortedMessagesSession an
  case result of
    Right messages -> pure messages
    Left err -> parseUsageError err

allAuthorSortedMessagesSession :: Text -> Session [Message]
allAuthorSortedMessagesSession authorName = Session.statement authorName allAuthorSortedMessages

allAuthorSortedMessages :: Statement Text [Message]
allAuthorSortedMessages = rmap tuplesToMessages [TH.vectorStatement| select content :: text, approved :: bool, author :: text from "messages" where author=$1::text|]
  where
    tupleToMessage (c, ap, au) = Message c ap au
    tuplesToMessages vec = V.toList $ V.map tupleToMessage vec


-----------------createMessage-----------------
createMessage :: (MonadDB m, MonadError ServerError m) =>  Message -> m Int64
createMessage m = do
  result <- runSession $ insertMessageSession m
  case result of
    Right idM -> pure idM
    Left err -> parseUsageError err

insertMessageSession :: Message -> Session Int64
insertMessageSession m = Session.statement (content m, author m) insertMessage

insertMessage :: Statement (Text, Text) Int64
insertMessage = [TH.singletonStatement| insert into "messages" (content, approved, author) values ($1 :: text, false, $2 :: text) returning id :: int8|]
