{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Wonsz.Storage.Postgres.Database
    ( initialize
    , PersistentBackendT(..)

    , runPostgresBackendT
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Maybe (listToMaybe)

import Control.Monad.IO.Unlift (MonadUnliftIO)

import Wonsz.Users.Domain (User(..))
import Wonsz.Users (UserMonad(..))

import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class  (liftIO, MonadIO(..))
import           Control.Monad.Logger    (runStderrLoggingT, MonadLogger, NoLoggingT(..))
import qualified Database.Persist               as Persist
import qualified Database.Persist.Sql           as PS
import qualified Database.Persist.Postgresql    as P
import qualified Database.Persist.Postgresql.JSON as PJSON
import Database.Esqueleto as E (from, where_, (^.), (==.), (=.), select, val, update, set, count) 
import qualified Database.Persist.TH            as PTH
import Servant (ServerError)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
    Account
        userId Int
        login Text
        name Text
        password ByteString
        deriving Show
|]

initialize :: MonadIO m => P.SqlPersistT m ()
initialize = P.runMigration migrateAll >> seedUsers

seedUsers :: MonadIO m => P.SqlPersistT m ()
seedUsers = do
    users <- select $ 
                from $ \account -> do
                    where_ $ account ^. AccountLogin ==. val "btk"
                    return $ account ^. AccountUserId
    if not (null users)
       then return ()
       else void $ P.insert $ Account 1 "btk" "Tomek" "password" 


newtype PersistentBackendT m a = PersistentBackendT 
    { unPersistentBackendT :: P.SqlPersistT m a } 
    deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

deriving newtype instance MonadError err m => MonadError err (PersistentBackendT m)

runPostgresBackendT
    :: MonadLogger m 
    => MonadUnliftIO m
    => P.ConnectionString 
    -> PersistentBackendT m a
    -> m a
runPostgresBackendT connectionString (PersistentBackendT persistT) = 
    P.withPostgresqlPool connectionString 10 $ \pool -> do 
        flip P.runSqlPool pool $ do
            initialize
            persistT 

runPostgresBackendTIO 
    :: P.ConnectionString 
    -> PersistentBackendT (NoLoggingT IO) a
    -> IO a
runPostgresBackendTIO conn = runNoLoggingT . runPostgresBackendT conn

instance (Monad m, MonadIO m) => UserMonad (PersistentBackendT m) where
    getUser userName = PersistentBackendT $ do
        matchingUser <- listToMaybe <$> findAccounts userName
        return $ mapToUser <$> matchingUser
      where
        mapToUser = User
            <$> accountUserId . P.entityVal
            <*> accountLogin . P.entityVal 
            <*> accountName . P.entityVal
            <*> accountPassword . P.entityVal

    saveUser (User uId _ uName uPassword) = PersistentBackendT $ 
        update $ \account -> do
            set account [ AccountName =. val uName, AccountPassword =. val uPassword]
            where_ $ account ^. AccountUserId ==. val uId

    getById userId = PersistentBackendT $ do
        matchingUser <- listToMaybe <$> findAccountById userId
        return $ mapToUser <$> matchingUser
      where
        mapToUser = User
            <$> accountUserId . P.entityVal
            <*> accountLogin . P.entityVal 
            <*> accountName . P.entityVal
            <*> accountPassword . P.entityVal

findAccounts :: MonadIO m => Text -> P.SqlPersistT m [P.Entity Account]
findAccounts userName = 
    select $ 
        from $ \account -> do
            where_ (account ^. AccountLogin ==. val userName) 
            return account

findAccountById :: MonadIO m => Int -> P.SqlPersistT m [P.Entity Account]
findAccountById userId = 
    select $ 
        from $ \account -> do
            where_ (account ^. AccountUserId ==. val userId) 
            return account
