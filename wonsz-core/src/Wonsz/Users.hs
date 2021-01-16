{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Wonsz.Users
    ( login
    , LoginCommand(..)

    , changePassword
    , ChangePasswordCommand(..)

    , UserDescription(..)
    , UserMonad(..)
    ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Control.Monad.Trans.Class (MonadTrans(..))

import Wonsz.Named (Named, name)
import Wonsz.Crypto (CryptoMonad(..))
import qualified Wonsz.Users.Domain as Domain 

data UserDescription = UserDescription
    { userId :: !Int
    , userName :: !Text
    }

type UserName = Text

class Monad m => UserMonad m where 
    getUser :: UserName -> m (Maybe Domain.User)
    getById :: Int -> m (Maybe Domain.User)
    saveUser :: Domain.User -> m ()

instance {-# OVERLAPPABLE #-}
    ( UserMonad m 
    , MonadTrans t 
    , Monad (t m)
    ) => UserMonad (t m) where 
    getUser = lift . getUser
    getById = lift . getById
    saveUser = lift . saveUser

data LoginCommand = LoginCommand
    { _loginUserName :: !Text
    , _loginPassword :: !Text
    } deriving (Eq, Show)

login 
    :: UserMonad m 
    => CryptoMonad m
    => LoginCommand
    -> m (Maybe UserDescription)
login LoginCommand{..} = do
    user <- getUser _loginUserName
    verified <- fromMaybe (return Nothing) $ Domain.verifyPassword <$> user <*> Domain.makePassword _loginPassword -- Maybe (m (Maybe User)) -> m (Maybe User)
    return $ UserDescription <$> (Domain.getUserId <$> verified) <*> (Domain.getUserName <$> verified)

data ChangePasswordCommand = ChangePasswordCommand
    { _changeeUserId :: !Int
    , _changerUserId :: !Int
    , _newPassword :: !Text
    }

changePassword 
    :: UserMonad m
    => CryptoMonad m
    => ChangePasswordCommand
    -> m ()
changePassword ChangePasswordCommand{..} = do
    maybeChanger <- getById _changerUserId
    maybeChangee <- getById _changeeUserId 

    case (maybeChanger, maybeChangee) of 
        (Just changer, Just changee) -> 
            name changer $ \namedChanger ->
                name changee $ \namedChangee ->
                    case (Domain.canChangePassword namedChanger namedChangee, Domain.makePassword _newPassword) of
                        (Nothing, _) -> return ()
                        (_, Nothing) -> return ()
                        (Just proof, Just newPassword) -> Domain.changePassword namedChanger namedChangee newPassword proof >>= saveUser

        (_, _) -> return ()
