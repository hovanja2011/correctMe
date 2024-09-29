module Handlers.Messages 
    ( getAllMessages
    , getMessageById
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
import           Control.Monad.IO.Class     (liftIO)


-----------------getAllMessages-----------------
getAllMessages :: (MonadDB m, MonadError ServerError m) => m [Message]
getAllMessages = do
  liftIO $ putStrLn "getAllMessages"
  result <- runSession allMessagesSession
  case result of
    Right messages -> pure messages
    Left err -> parseUsageError err

allMessagesSession :: Session [Message]
allMessagesSession = Session.statement () allMessages

allMessages :: Statement () [Message]
allMessages = rmap tuplesToMessages [TH.vectorStatement| select id :: int8, content :: text, author :: text, approved :: bool from "messages"|]
  where
    tupleToMessage (i, c, au, ap) = Message i c au ap
    tuplesToMessages vec = V.toList $ V.map tupleToMessage vec

-----------------getMessageById-----------------
getMessageById :: (MonadDB m, MonadError ServerError m) => IdentM -> m Message
getMessageById i = do
  liftIO $ putStrLn "getMessageById"
  result <- runSession $ messageIdSession (identM i)
  case result of
    Right messages -> pure messages
    Left err -> parseUsageError err

messageIdSession :: Int64 -> Session Message
messageIdSession i = Session.statement i idMessage

idMessage :: Statement Int64 Message
idMessage = rmap tupleToMessage [TH.singletonStatement| select id :: int8, content :: text, author :: text, approved :: bool from "messages" where id = $1 :: int8|]
  where
    tupleToMessage (i, c, au, ap) = Message i c au ap

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
allApproveSortedMessages = rmap tuplesToMessages [TH.vectorStatement| select id :: int8, content :: text, author :: text , approved :: bool from "messages" where approved = $1 :: bool|]
  where
    tupleToMessage (i, c, au, ap) = Message i c au ap
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
allAuthorSortedMessages = rmap tuplesToMessages [TH.vectorStatement| select id :: int8, content :: text, author :: text , approved :: bool from "messages" where author= $1 :: text|]
  where
    tupleToMessage (i, c, au, ap) = Message i c au ap
    tuplesToMessages vec = V.toList $ V.map tupleToMessage vec


-----------------createMessage-----------------
createMessage :: (MonadDB m, MonadError ServerError m) =>  Message -> m IdentM
createMessage m = do
  liftIO $ putStrLn "createMessage"
  result <- runSession $ insertMessageSession m
  case result of
    Right idM -> pure idM
    Left err -> parseUsageError err

insertMessageSession :: Message -> Session IdentM
insertMessageSession m = Session.statement (content m, author m, approved m) insertMessage

insertMessage :: Statement (Text, Text, Bool) IdentM
insertMessage = rmap intToIdent [TH.singletonStatement| insert into "messages" (content, author, approved) values ($1 :: text, $2 :: text, $3 :: bool) returning id :: int8|]
  where
    intToIdent (i) = IdentM i