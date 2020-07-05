{-# LANGUAGE RecordWildCards #-}

module Wonsz.Users
    ( login
    , UserMonad(..)
    , CryptoSettings(..)
    , UserDescription(..)
    , User(..)
    ) where

newtype CryptoSettings = CryptoSettings { hash :: String -> String }

type UserName = String
type Password = String

data User = User
    { uId :: !Int
    , uName :: !String
    , uPassword :: !String
    }

data UserDescription = UserDescription
    { userId :: !Int
    , userName :: !String
    }

class Monad m => UserMonad m where 
  getUser :: UserName -> m (Maybe User)
  getUser userName = return $ Just $ User 1 userName "password"

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
        if uPassword == hash password 
               then Just $ UserDescription uId uName
               else Nothing
