{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TemplateHaskell, TypeFamilies, FlexibleContexts, OverloadedStrings #-}
module Model (Name, Person, name, PeopleDb(PeopleDb), AllPeople(AllPeople), LookupPerson(LookupPerson), SetPerson(SetPerson), samplePerson) where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState, get, put)
import Data.Acid (Query, Update, makeAcidic)
import Data.SafeCopy (deriveSafeCopy, base)
import GHC.Generics (Generic)

import Data.Text (Text())

import Servant.Docs (ToSample, toSamples, singleSample)
import Data.Aeson.Types (ToJSON, FromJSON)

import qualified Data.Map.Strict as Map

type Name = Text

data Person = Person {
  name :: Name,
  age :: Int
} deriving (Show, Generic, ToJSON, FromJSON)

samplePerson = Person "Bob" 40

instance ToSample Person where
  toSamples _ = singleSample samplePerson

newtype PeopleDb = PeopleDb { peopleMap :: Map.Map Name Person }

deriveSafeCopy 0 'base ''Person
deriveSafeCopy 0 'base ''PeopleDb

allPeople :: MonadReader PeopleDb m => m [Person]
allPeople = asks $ Map.elems . peopleMap

lookupPerson :: MonadReader PeopleDb m => Name -> m (Maybe Person)
lookupPerson name = asks $ Map.lookup name . peopleMap

setPerson :: MonadState PeopleDb m => Name -> Person -> m Bool
setPerson name person = do
  PeopleDb db <- get
  put $ PeopleDb $ Map.insert name person db
  pure $ Map.member name db

makeAcidic ''PeopleDb ['lookupPerson, 'allPeople, 'setPerson]
