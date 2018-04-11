{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell, DataKinds, TypeFamilies, TypeOperators, TypeApplications, FlexibleContexts #-}
module App where

import           Control.Monad.Reader
import           Control.Monad.State
import Control.Monad.Error.Class
import Data.Either.Combinators (maybeToRight)
import           Data.Acid
import           Data.SafeCopy
import           Data.Typeable
import GHC.Generics

--import Servant.API
import Servant
import Data.Aeson.Types
--import Data.Aeson.Compat
import Network.Wai.Handler.Warp (run)

import qualified Data.Map.Strict as Map


maybeError :: MonadError e m => e -> Maybe a -> m a
maybeError err = liftEither . maybeToRight err

type Name = String

data Person = Person {
  name :: Name,
  age :: Int
} deriving (Show, Typeable, Generic)

instance ToJSON Person

person = Person "Leif" 38
people = [person]

newtype PeopleDb = PeopleDb { peopleMap :: Map.Map Name Person }

$(deriveSafeCopy 0 'base ''Person)
$(deriveSafeCopy 0 'base ''PeopleDb)

lookupPerson :: Name -> Query PeopleDb (Maybe Person)
lookupPerson name = asks $ Map.lookup name . peopleMap
--lookupPerson name = asks (\(PeopleDb peopleMap) -> Map.lookup name peopleMap)

allPeople :: Query PeopleDb [Person]
allPeople = asks $ Map.elems . peopleMap

$(makeAcidic ''PeopleDb ['lookupPerson, 'allPeople])


--type PeopleIndex = "people" :> Get '[JSON] [Person]
type PeopleIndex = Get '[JSON] [Person]
type SpecificPerson = Capture "name" Name :> Get '[JSON] Person

type PeopleAPI = "people" :> (PeopleIndex :<|> SpecificPerson)

foo :: MonadIO m => m [Person]
foo = pure people

--bar :: (MonadIO m, MonadError ServantErr m) => Name -> m Person
--bar name = if name == "Leif" then pure person else throwError err404
bar :: (MonadIO m, MonadError ServantErr m) => AcidState PeopleDb -> Name -> m Person
bar db name = liftIO (query db (LookupPerson name)) >>= maybeError err404

liftedQuery :: (MonadIO m, QueryEvent event, MonadReader (AcidState (EventState event)) m) => event -> m (EventResult event)
liftedQuery q = do
  db <- ask
  liftIO $ query db q

peopleIndex :: Server PeopleIndex
peopleIndex = foo

specificPerson :: Server SpecificPerson
--specificPerson = bar
--specificPerson db name = 
specificPerson name = if name == "Leif" then pure person else throwError err404

peopleAPI :: Server PeopleAPI
--peopleAPI = peopleIndex :<|> specificPerson
peopleAPI = foo :<|> specificPerson

app :: Application
app = serve @PeopleAPI Proxy peopleAPI

--dbIOToHandler :: (MonadIO m, QueryEvent event, MonadReader (AcidState (EventState event)) m) 
--dbIOToHandler :: (MonadIO m, IsAcidic db, MonadReader (AcidState db) m) => db -> m (Maybe a) -> Handler a
--dbIOToHandler :: (MonadIO m, IsAcidic db, MonadReader (AcidState db) m) => db -> m (Maybe a) -> Handler a
--dbIOToHandler db action = runReaderT db action

--dbIOToHandler :: (MonadIO m, MonadReader (AcidState PeopleDb) m) => AcidState PeopleDb -> m (a) -> Handler a
--dbIOToHandler db action = liftIO $ runReaderT action db

dbIOToHandler :: AcidState PeopleDb -> ReaderT (AcidState PeopleDb) IO (Maybe a) -> Handler a
dbIOToHandler db action = liftIO (runReaderT action db) >>= maybeError err404

--dbIOToHandler :: MonadIO m => AcidState PeopleDb -> ReaderT (AcidState PeopleDb) m (Maybe a) -> Handler a
--dbIOToHandler db action = (runReaderT action db) >>= maybeError err404

main :: IO ()
main = do
  db <- openLocalState (PeopleDb $ Map.fromList [("Leif", Person "Leif" 38)])
  run 3000 app
  closeAcidState db
