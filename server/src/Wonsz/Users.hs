{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Wonsz.Users
    ( login
    , UserMonad(..)
    , CryptoSettings(..)
    , UserDescription(..)
    , User(..)

    , ChangePasswordRequest(..)
    , changePassword 
    , canChangePassword 

    , Id(..)
    ) where

import Data.ByteString.Lazy
import Data.Text

import Wonsz.Named (Named, unNamed)

newtype CryptoSettings = CryptoSettings { hash :: ByteString -> ByteString }

type UserName = String
type Password = ByteString

data User = User
    { _uId :: Int
    , _userName :: !String
    , _password :: ByteString
    , _givenName :: !String
    , _familyName :: !String
    } deriving (Show, Read)

data UserDescription = UserDescription
    { userId :: !Int
    , userName :: !String
    }

class Monad m => UserMonad m where 
    getUser :: UserName -> m (Maybe User)
    getById :: Int -> m (Maybe User)
    saveUser :: User -> m ()

login 
    :: UserMonad m 
    => CryptoSettings 
    -> UserName 
    -> Password 
    -> m (Maybe UserDescription)
login CryptoSettings{..} userName password = 
    (>>= checkPassword password) <$> getUser userName 
  where
    checkPassword password u@User{..} =
        if _password == hash password 
               then Just $ UserDescription _uId _userName
               else Nothing

newtype Id x = Id { unId :: Int }

data ChangePasswordRequest = ChangePasswordRequest 
    { xId :: Id User
    , newPassword :: Password
    }

newtype CanChangePassword changerUser changeeUser = CanChangePassword User

canChangePassword 
    :: UserMonad m 
    => Named changerUserId Int
    -> Named changeeUserId ChangePasswordRequest
    -> m (Maybe (changerUserId `CanChangePassword` changeeUserId))
canChangePassword changerUserId changeRequest = 
    if unNamed changerUserId == (unId . xId . unNamed) changeRequest then fmap CanChangePassword <$> getById ((unId . xId . unNamed) changeRequest) else return Nothing

changePassword 
    :: CryptoSettings 
    -> Named changerUserId Int
    -> Named changeeUser ChangePasswordRequest
    -> changerUserId `CanChangePassword` changeeUser
    -> User
changePassword CryptoSettings{..} _ request (CanChangePassword user) = user { _password = pswdHash }
    where ChangePasswordRequest _ newPswd = unNamed request
          pswdHash = hash newPswd
