{-# LANGUAGE  FlexibleContexts, ConstraintKinds #-}

module AcidReader (AcidIO, liftedQuery, liftedUpdate) where

import Control.Monad.Reader (MonadIO, MonadReader, ask, liftIO)
import Data.Acid

type AcidIO event m = (MonadIO m, MonadReader (AcidState (EventState event)) m)

liftedQuery :: (QueryEvent event, AcidIO event m) => event -> m (EventResult event)
liftedQuery q = do
  db <- ask
  liftIO $ query db q

liftedUpdate :: (UpdateEvent event, AcidIO event m) => event -> m (EventResult event)
liftedUpdate q = do
  db <- ask
  liftIO $ update db q
