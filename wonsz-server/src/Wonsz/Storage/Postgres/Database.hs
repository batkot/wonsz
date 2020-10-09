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

module Wonsz.Storage.Postgres.Database
    (
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import qualified Database.Persist               as Persist
import qualified Database.Persist.Postgresql    as P
import qualified Database.Persist.Postgresql.JSON as PJSON
import qualified Database.Persist.TH            as PTH

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
    User
        userId ByteString
        login Text
        name Text
        password ByteString
        deriving Show
|]

-- initialize :: P.ConnectionString -> IO ()
-- initialize connStr = runStderrLoggingT $ P.withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
--     flip P.runSqlPersistMPool pool $ do
--         P.runMigration migrateAll
--         _ <- Persist.insert $ Person "John" (Just 50)
--         _ <- Persist.insert $ Person "John 2" (Just 50)
--         _ <- Persist.insert $ Event "ABCD" 0 $ EJ.JSONB $ Stuff "Some text" 10
--         _ <- Persist.insert $ Event "ABCD" 0 $ EJ.JSONB $ Stuff "Some text 2" 10
--         _ <- Persist.insert $ Event "ABCD" 0 $ EJ.JSONB $ Stuff "Some text 2" 20
--         E.update $ \p -> do
--             E.set p [ PersonName E.=. E.val "Dupa" ]
--             E.where_ (p E.^. PersonName E.==. E.val "John")

--         E.update $ \ev -> do
--             E.set ev [ EventJson E.=. E.val (EJ.JSONB (Stuff "New text" 12)) ]
--             E.where_ (E.just (ev E.^. EventJson) EJ.->. "field" EJ.?. "Some text")
        
--         return ()
