{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Wonsz.Storage.InMemory.Repositories
    where

import Control.Monad.Identity (IdentityT(..))
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error.Class (MonadError)

import Data.Text (pack, unpack)

import Wonsz.Users.Domain (User(..))
import Wonsz.Users (UserMonad(..))
import Wonsz.Storage

newtype KvsUserMonadT m a = KvsUserMonadT { runKvsUserMonadT :: m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)
    deriving MonadTrans via IdentityT

deriving newtype instance MonadError err m => MonadError err (KvsUserMonadT m)

instance (Monad m, KeyValueStorage m String User) => UserMonad (KvsUserMonadT m) where 
    getUser = KvsUserMonadT . get . unpack
    saveUser user = KvsUserMonadT $ do 
        set ((unpack . _userLogin) user) user
        set ((show . _userId) user) user
    getById = KvsUserMonadT . get . show 
