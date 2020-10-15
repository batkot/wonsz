{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Wonsz.Server.Account
    ( AccountApi
    , accountApi

    , ChangePasswordRequest(..)
    ) where

import qualified Data.Text as Text
import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

import Control.Monad.Error.Class (MonadError, throwError)

import Servant (JSON, ReqBody, Capture, Get, Post, (:>), (:<|>)(..), err401, err403, err404, ServerError, ServerT, Handler)
import Servant.Auth.Server (AuthResult(..), Auth, ThrowAll(..))

import Wonsz.Server.Authentication (Protected, protected, AuthenticatedUser, getAuthenticatedUserId, protected2)

import Wonsz.Users 
import Wonsz.Users.Domain (getUserId, getUserName) -- tmp
import Wonsz.Crypto (CryptoMonad)

type AccountApi auth = "account" :> Protected auth :> AccountApi'

type AccountApi' = 
        Capture "id" Int :> Get '[JSON] AccountDetails
        :<|> "changePassword" :> ReqBody '[JSON] ChangePasswordRequest :> Post '[JSON] ()

data AccountDetails = AccountDetails
    { accountId :: !Int
    , accountName :: !String
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ChangePasswordRequest = ChangePasswordRequest 
    { newPassword :: !String 
    , currentPassword :: !String
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

accountApi
    :: UserMonad m 
    => MonadError ServerError m
    => CryptoMonad m
    => AuthResult AuthenticatedUser
    -> ServerT AccountApi' m 
accountApi (Authenticated user) = accountDetailsHandler user :<|> changePasswordHandler user
accountApi _ = const (throwError err401) :<|> const (throwError err401)

changePasswordHandler
    :: UserMonad m 
    => CryptoMonad m
    => AuthenticatedUser 
    -> ChangePasswordRequest
    -> m ()
changePasswordHandler user ChangePasswordRequest{..} = changePassword command
  where 
    userId = getAuthenticatedUserId user
    command = ChangePasswordCommand userId userId (Text.pack newPassword)

accountDetailsHandler 
    :: UserMonad m
    => MonadError ServerError m
    => AuthenticatedUser
    -> Int 
    -> m AccountDetails
accountDetailsHandler _ userId = do
    user <- getById userId
    case user of
        Nothing -> throwError err404
        Just u -> return $ AccountDetails (getUserId u) ((Text.unpack . getUserName) u)
