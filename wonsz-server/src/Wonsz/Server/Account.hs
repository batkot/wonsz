{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Wonsz.Server.Account
    ( AccountApi

    , ChangePasswordRequest(..)
    ) where

import qualified Data.Text as Text
import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

import Control.Monad.Error.Class (MonadError, throwError)

import Servant (JSON, ReqBody, Capture, Get, Post, (:>), (:<|>)(..), err401, err403, ServerError, ServerT, Handler)
import Servant.Auth.Server (AuthResult(..), Auth)

import Wonsz.Server.Authentication (Protected, protected, AuthenticatedUser, getAuthenticatedUserId)

import Wonsz.Users 
import Wonsz.Crypto (CryptoMonad)

type AccountApi auth = 
    Protected auth :> Capture "id" Int :> Get '[JSON] ()
    :<|> Protected auth :> "changePassword" :> ReqBody '[JSON] ChangePasswordRequest :> Post '[JSON] ()

data ChangePasswordRequest = ChangePasswordRequest 
    { newPassword :: !String 
    , currentPassword :: !String
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

tmpChangePasswordHandler 
    :: UserMonad m 
    => CryptoMonad m
    => MonadError ServerError m
    => AuthResult AuthenticatedUser
    -> ChangePasswordRequest
    -> m ()
tmpChangePasswordHandler (Authenticated user) = changePasswordHandler user 
tmpChangePasswordHandler _ = const $ throwError err401

changePasswordHandler
    :: UserMonad m 
    => CryptoMonad m
    => MonadError ServerError m
    => AuthenticatedUser 
    -> ChangePasswordRequest
    -> m ()
changePasswordHandler user ChangePasswordRequest{..} = changePassword command
  where 
    userId = getAuthenticatedUserId user
    command = ChangePasswordCommand userId userId (Text.pack newPassword)
