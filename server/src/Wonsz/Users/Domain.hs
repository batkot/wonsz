{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}

module Wonsz.Users.Domain 
    ( User(..) -- hot wired in Main.hs, fix it
    -- use optics
    , getUserId 
    , getUserName 

    , HashingAlgorithm (..) 
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
newtype HashingAlgorithm = HashingAlgorithm { hash :: ByteString -> ByteString }

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

verifyPassword :: HashingAlgorithm -> User -> Password -> Maybe User
verifyPassword (HashingAlgorithm hash) user (Password password) =
    if hash  password == _passwordHash user
       then Just user
       else Nothing

newtype CanChangePassword changer changee = CanChangePassword User

canChangePassword :: Named changer User -> Named changee User -> Maybe (changer `CanChangePassword` changee)
canChangePassword (Named changer) (Named changee) 
  | _userId changer == _userId changee    = Just $ CanChangePassword changee
  | otherwise = Nothing

changePassword 
    :: HashingAlgorithm 
    -> Named changer User 
    -> Named changee User 
    -> Password
    -> changer `CanChangePassword` changee
    -> User
changePassword (HashingAlgorithm  hash) _ (Named changee) (Password newPassword) proof = 
    changee { _passwordHash = hash newPassword }
