{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.Persist.Class where

import Data.Aeson
import Reflex
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Reader (ReaderT)

class Persist t m where
  persistE' :: (FromJSON a, ToJSON a)
             => Event t a
             -> m (Maybe a)

persistE :: (Persist t m, FromJSON a, ToJSON a, Functor m) => a -> Event t a -> m a
persistE i = fmap (fromMaybe i) . persistE'

instance (Monad m, Persist t m) => Persist t (EventWriterT t w m) where
  persistE' = lift . persistE'

instance (Monad m, Persist t m) => Persist t (ReaderT r m) where
  persistE' = lift . persistE'

