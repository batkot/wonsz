{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}

module Wonsz.Users.Domain 
    ( User(..) -- hot wired in Main.hs, fix it
    -- use optics
    , getUserId 
    , getUserName 
    , getUserLogin

    , Password
    , makePassword
    , verifyPassword

    , CanChangePassword 
    , canChangePassword 
    , changePassword
    ) where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)

import Wonsz.Named (Named, pattern Named)
import Wonsz.Crypto (CryptoMonad(..))


-- User domain representation
-- actions: 
--  Login/VerifyPassword -> return Some user info
--  ChangePassword 

newtype Password = Password { unPassword :: ByteString } 

makePassword :: Text -> Maybe Password
makePassword  = Just . Password . encodeUtf8

data User = User
    { _userId :: !Int
    , _userLogin :: !Text
    , _userName :: !Text
    , _passwordHash :: !ByteString
    } deriving stock (Eq, Show)

getUserId :: User -> Int
getUserId = _userId

getUserName :: User -> Text
getUserName = _userName

getUserLogin :: User -> Text
getUserLogin = _userLogin

verifyPassword 
    :: CryptoMonad m
    => User 
    -> Password 
    -> m (Maybe User)
verifyPassword user (Password password) = do
    pswd <- hash password
    if pswd == _passwordHash user
       then return $ Just user
       else return Nothing

newtype CanChangePassword changer changee = CanChangePassword User

canChangePassword :: Named changer User -> Named changee User -> Maybe (changer `CanChangePassword` changee)
canChangePassword (Named changer) (Named changee) 
  | _userId changer == _userId changee    = Just $ CanChangePassword changee
  | otherwise = Nothing

changePassword 
    :: CryptoMonad m
    => Named changer User 
    -> Named changee User 
    -> Password
    -> changer `CanChangePassword` changee
    -> m User
changePassword _ (Named changee) (Password newPassword) proof = do
    newPswd <- hash newPassword
    return $ changee { _passwordHash = newPswd }
