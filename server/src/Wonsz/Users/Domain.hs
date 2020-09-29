{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}

module Wonsz.Users.Domain 
    ( User
    , HashingAlgorythm (..)
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

-- User domain representation
-- actions: 
--  Login/VerifyPassword -> return Some user info
--  ChangePassword 

newtype Password = Password { unPassword :: ByteString } 

makePassword :: Text -> Maybe Password
makePassword  = Just . Password . encodeUtf8

newtype Login = Login { unLogin :: Text } 
newtype HashingAlgorythm = HashingAlgorythm { hash :: ByteString -> ByteString }

data User = User
    { _userId :: !Int
    , _userLogin :: !Text
    , _userName :: !Text
    , _passwordHash :: !ByteString
    } deriving stock (Eq, Show)

verifyPassword :: HashingAlgorythm -> User -> Password -> Maybe User
verifyPassword (HashingAlgorythm hash) user (Password password) =
    if hash  password == _passwordHash user
       then Just user
       else Nothing

newtype CanChangePassword changer changee = CanChangePassword User

canChangePassword :: Named changer User -> Named changee User -> Maybe (changer `CanChangePassword` changee)
canChangePassword (Named changer) (Named changee) 
  | _userId changer == _userId changee    = Just $ CanChangePassword changee
  | otherwise = Nothing

changePassword 
    :: HashingAlgorythm 
    -> Named changer User 
    -> Named changee User 
    -> Password
    -> changer `CanChangePassword` changee
    -> User
changePassword (HashingAlgorythm  hash) _ (Named changee) (Password newPassword) proof = 
    changee { _passwordHash = hash newPassword }
