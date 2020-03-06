{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, RankNTypes #-}
module ServantAcidGlue (ReaderAcidHandler, serveReader, maybeError) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Error.Class (MonadError, liftEither)
import Data.Either.Combinators (maybeToRight)
import Data.Acid (AcidState)
import Servant

-- From http://hackage.haskell.org/package/natural-transformation-0.4/docs/Control-Natural.html#t:-126--62-
infixr 0 ~>
type f ~> g = forall x. f x -> g x

-- Given an error message as a fallback, lift a Maybe into the MonadError
maybeError :: MonadError e m => e -> Maybe a -> m a
maybeError err = liftEither . maybeToRight err

type ReaderHandler s = ReaderT s Handler
type ReaderAcidHandler s = ReaderHandler (AcidState s)

-- This seems like it could be upstreamed into Servant?
serveMonadStack :: HasServer api '[] => Proxy api -> (m ~> Handler) -> ServerT api m -> Application
serveMonadStack p nt = serve p . hoistServer p nt

serveReader :: HasServer api '[] => Proxy api -> r -> ServerT api (ReaderHandler r) -> Application
serveReader p r = serveMonadStack p (flip runReaderT r)
