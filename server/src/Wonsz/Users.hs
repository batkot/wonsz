{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Wonsz.Users
    ( login
    , LoginCommand(..)

    , changePassword
    , ChangePasswordCommand(..)

    , UserDescription(..)
    , UserMonad(..)
    ) where

import Data.Text (Text)

import Wonsz.Named (Named, name)
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

data LoginCommand = LoginCommand
    { _loginUserName :: !Text
    , _loginPassword :: !Text
    } deriving (Eq, Show)

login 
    :: UserMonad m 
    => Domain.HashingAlgorithm
    -> LoginCommand
    -> m (Maybe UserDescription)
login hash LoginCommand{..} = do
    user <- getUser _loginUserName
    return $ do 
        u <- user
        p <- Domain.makePassword _loginPassword
        verified <- Domain.verifyPassword hash u p
        return $ UserDescription (Domain.getUserId verified) (Domain.getUserName verified)

data ChangePasswordCommand = ChangePasswordCommand
    { _changeeUserId :: !Int
    , _changerUserId :: !Int
    , _newPassword :: !Text
    }

changePassword 
    :: UserMonad m
    => Domain.HashingAlgorithm
    -> ChangePasswordCommand
    -> m ()
changePassword hash ChangePasswordCommand{..}= do
    maybeChanger <- getById _changerUserId
    maybeChangee <- getById _changeeUserId 

    case (maybeChanger, maybeChangee) of 
        (Just changer, Just changee) -> 
            name changer $ \namedChanger ->
                name changee $ \namedChangee ->
                    case (Domain.canChangePassword namedChanger namedChangee, Domain.makePassword _newPassword) of
                        (Nothing, _) -> return ()
                        (_, Nothing) -> return ()
                        (Just proof, Just newPassword) -> saveUser $ Domain.changePassword hash namedChanger namedChangee newPassword proof 

        (_, _) -> return ()
