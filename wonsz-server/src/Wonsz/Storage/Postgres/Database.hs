{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Wonsz.Storage.Postgres.Database
    ( initialize
    , initializePostgresqlPool

    , PersistentSqlBackEndT (..)
    , runPostgresBackEndT
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Maybe (listToMaybe)

import Control.Monad.IO.Unlift (MonadUnliftIO)

import Wonsz.Users.Domain (User(..))
import qualified Wonsz.Scoreboard.Domain as WSD
import Wonsz.Users (UserMonad(..))

import Wonsz.Server.Dashboard (DashboardQueries(..), ScoreboardSummary(..))
import qualified Wonsz.Server.Account as WSA

import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Identity (IdentityT(..))
import           Control.Monad.IO.Class  (liftIO, MonadIO(..))
import           Control.Monad.Logger    (MonadLoggerIO)
import qualified Database.Persist               as Persist
import qualified Database.Persist.Sql           as PS
import qualified Database.Persist.Postgresql    as P
import qualified Database.Persist.Postgresql.JSON as PJSON
import Database.Esqueleto as E (from, where_, (&&.), (^.), (==.), (=.), select, val, update, set, countRows, count, countDistinct, SqlExpr, Value(..), exists, valkey)
-- import Database.Esqueleto.Experimental
import qualified Database.Persist.TH            as PTH

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
    Account
        userId Int
        login Text
        name Text
        password ByteString
        deriving Show

    Scoreboard
        scoreboardId Int
        name Text

    Participant
        userId AccountId
        scoreboardId ScoreboardId
|]

initializePostgresqlPool
    :: MonadUnliftIO m
    => MonadLoggerIO m
    => P.ConnectionString
    -> Int
    -> m P.ConnectionPool
initializePostgresqlPool connString poolSize = do
    pool <- P.createPostgresqlPool connString poolSize
    P.runSqlPool initialize pool
    return pool

initialize :: MonadIO m => P.SqlPersistT m ()
initialize = P.runMigration migrateAll >> seedUsers

seedUsers :: MonadIO m => P.SqlPersistT m ()
seedUsers = do
    usersCount <- select $
                from $ \account -> do
                    where_ $ account ^. AccountLogin ==. val "btk"
                    return (countRows :: SqlExpr (Value Int))
    if sum (unValue <$> usersCount) > 0
       then return ()
       else void $ P.insert $ Account 1 "btk" "Tomek" "password"

newtype PersistentSqlBackEndT m a = PersistentSqlBackEndT
    { unPersistentSqlBackEndT :: ReaderT P.ConnectionPool m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

deriving newtype instance MonadError err m => MonadError err (PersistentSqlBackEndT m)

runSql
    :: Monad m
    => MonadIO m
    => P.SqlPersistT IO a -> PersistentSqlBackEndT m a
runSql sql = PersistentSqlBackEndT $ do
    pool <- ask
    liftIO $ P.runSqlPool sql pool

runPostgresBackEndT :: P.ConnectionPool -> PersistentSqlBackEndT m a -> m a
runPostgresBackEndT pool = flip runReaderT pool . unPersistentSqlBackEndT

instance (Monad m, MonadIO m) => DashboardQueries (PersistentSqlBackEndT m) where
    getUserDashboard userId = do
        scoreboards <- runSql $ findUserScoreboards userId
        return $ createScoreboardSummary . P.entityVal <$> scoreboards

createScoreboardSummary :: Scoreboard -> ScoreboardSummary
createScoreboardSummary (Scoreboard id name) = ScoreboardSummary id name 0 $ WSA.AccountDetails 1 "btk" "Tomek" ""

findUserScoreboards :: MonadIO m => Int -> P.SqlPersistT m [P.Entity Scoreboard]
findUserScoreboards userId = 
    select $ 
        from $ \s -> do
            where_ $ exists $
                from $ \(p, acc) -> 
                    where_ $ 
                        (p ^. ParticipantScoreboardId ==. s ^. ScoreboardId) 
                        &&. (p ^. ParticipantUserId ==. acc ^. AccountId)
                        &&. (acc ^. AccountUserId ==. val userId)
            return s

instance (Monad m, MonadIO m) => UserMonad (PersistentSqlBackEndT m) where
    getUser userName = do
        matchingUser <- listToMaybe <$> runSql (findAccounts userName)
        return $ mapToUser <$> matchingUser
      where
        mapToUser = User
            <$> accountUserId . P.entityVal
            <*> accountLogin . P.entityVal
            <*> accountName . P.entityVal
            <*> accountPassword . P.entityVal

    saveUser (User uId _ uName uPassword) = runSql $
            update $ \account -> do
                set account [ AccountName =. val uName, AccountPassword =. val uPassword]
                where_ $ account ^. AccountUserId ==. val uId

    getById userId = runSql $ do
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
