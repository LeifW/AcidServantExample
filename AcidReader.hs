{-# LANGUAGE  FlexibleContexts, ConstraintKinds #-}

module AcidReader (AcidIO, liftedQuery, liftedUpdate) where

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Data.Acid
import Data.Has

type AcidIO r event m = (MonadIO m, MonadReader r m, Has (AcidState (EventState event)) r)

liftedQuery :: (QueryEvent event, AcidIO r event m) => event -> m (EventResult event)
liftedQuery q = do
  db <- asks getter
  liftIO $ query db q

liftedUpdate :: (UpdateEvent event, AcidIO r event m) => event -> m (EventResult event)
liftedUpdate q = do
  db <- asks getter
  liftIO $ update db q
