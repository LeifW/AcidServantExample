{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass, TemplateHaskell, DataKinds, TypeFamilies, TypeOperators, TypeApplications, FlexibleContexts, FlexibleInstances, RankNTypes, ConstraintKinds, OverloadedStrings #-}
module App where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error.Class (MonadError, liftEither)
import Data.Either.Combinators (maybeToRight)
import Data.Acid
import Data.SafeCopy
import Data.Typeable
import GHC.Generics

import Data.Text (Text())

import Servant
import Servant.Docs
import Data.Aeson.Types
import Network.Wai.Handler.Warp (run)

import Control.Exception.Base (bracket)

import qualified Data.Map.Strict as Map
import Data.Functor (($>))

-- From http://hackage.haskell.org/package/natural-transformation-0.4/docs/Control-Natural.html#t:-126--62-
infixr 0 ~>
type f ~> g = forall x. f x -> g x

maybeError :: MonadError e m => e -> Maybe a -> m a
maybeError err = liftEither . maybeToRight err

type Name = Text

data Person = Person {
  name :: Name,
  age :: Int
} deriving (Show, Typeable, Generic, ToJSON, FromJSON)

samplePerson = Person "Bob" 40

instance ToSample Person where
  toSamples _ = singleSample samplePerson

newtype PeopleDb = PeopleDb { peopleMap :: Map.Map Name Person }

deriveSafeCopy 0 'base ''Person
deriveSafeCopy 0 'base ''PeopleDb

lookupPerson' :: MonadReader PeopleDb m => Name -> m (Maybe Person)
lookupPerson' name = asks $ Map.lookup name . peopleMap

lookupPerson :: Name -> Query PeopleDb (Maybe Person)
lookupPerson = lookupPerson'

allPeople :: Query PeopleDb [Person]
allPeople = asks $ Map.elems . peopleMap

setPerson' :: MonadState PeopleDb m => Name -> Person -> m Bool
setPerson' name person = do
  PeopleDb db <- get
  put $ PeopleDb $ Map.insert name person db
  pure $ Map.member name db

setPerson :: Name -> Person -> Update PeopleDb Bool
setPerson = setPerson'

makeAcidic ''PeopleDb ['lookupPerson, 'allPeople, 'setPerson]

type NameSegment = Capture "name" Name

type PeopleIndex = Get '[JSON] [Person]
type SpecificPerson = NameSegment :> Get '[JSON] Person
type UpdateSpecificPerson = NameSegment:> ReqBody '[JSON] Person :> Put '[JSON] Person

instance ToCapture NameSegment where
  toCapture _ = DocCapture "name" "Name of the person being referenced."

type PeopleAPI = "people" :> (
    PeopleIndex :<|>
    SpecificPerson :<|>
    UpdateSpecificPerson
  )

type AcidIO event m = (MonadIO m, MonadReader (AcidState (EventState event)) m)

liftedQuery :: (QueryEvent event, AcidIO event m) => event -> m (EventResult event)
liftedQuery q = do
  db <- ask
  liftIO $ query db q

liftedUpdate :: (UpdateEvent event, AcidIO event m) => event -> m (EventResult event)
liftedUpdate q = do
  db <- ask
  liftIO $ update db q

peopleIndex :: PeopleServer PeopleIndex
peopleIndex = liftedQuery AllPeople

specificPerson :: PeopleServer SpecificPerson
specificPerson name = liftedQuery (LookupPerson name) >>= maybeError err404

updateSpecificPerson :: PeopleServer UpdateSpecificPerson
updateSpecificPerson name person = liftedUpdate (SetPerson name person) $> person

type ReaderHandler s = ReaderT s Handler
type ReaderAcidHandler s = ReaderHandler (AcidState s)
type PeopleServer api = ServerT api (ReaderAcidHandler PeopleDb)

peopleAPI :: PeopleServer PeopleAPI
peopleAPI =
  peopleIndex :<|>
  specificPerson :<|>
  updateSpecificPerson

serveMonadStack :: HasServer api '[] => Proxy api -> (m ~> Handler) -> ServerT api m -> Application
serveMonadStack p nt = serve p . hoistServer p nt

serveReader :: HasServer api '[] => Proxy api -> r -> ServerT api (ReaderHandler r) -> Application
serveReader p r = serveMonadStack p (flip runReaderT r)
  
app :: AcidState PeopleDb -> Application
app db = serveReader @PeopleAPI Proxy db peopleAPI

main :: IO ()
main = bracket
  (openLocalState (PeopleDb $ Map.fromList [(name samplePerson, samplePerson)]))
  (closeAcidState)
  (run 3000 . app)
