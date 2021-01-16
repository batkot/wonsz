{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Wonsz.Crypto
    ( CryptoMonad(..)

    , PlainTextCryptoT(..)
    ) where

import Data.ByteString (ByteString)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Identity (IdentityT(..))

class Monad m => CryptoMonad m where
    hash :: ByteString -> m ByteString

instance {-# OVERLAPPABLE #-}
    ( CryptoMonad m
    , MonadTrans t
    , Monad (t m)) => CryptoMonad (t m) where
    hash = lift . hash

newtype PlainTextCryptoT m a = PlainTextCryptoT
    { runPlainTextCryptoT :: m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)
    deriving MonadTrans via IdentityT


instance Monad m => CryptoMonad (PlainTextCryptoT m) where
    hash = return
