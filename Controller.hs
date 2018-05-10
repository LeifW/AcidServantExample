{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, TypeApplications, FlexibleInstances #-}
module Controller where

import Data.Acid (AcidState)
import Servant
import Servant.Docs

import Data.Functor (($>))

import Model
import AcidReader
import ServantAcidGlue

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

type PeopleServer api = ServerT api (ReaderAcidHandler PeopleDb)

peopleIndex :: PeopleServer PeopleIndex
peopleIndex = liftedQuery AllPeople

specificPerson :: PeopleServer SpecificPerson
specificPerson name = liftedQuery (LookupPerson name) >>= maybeError err404

updateSpecificPerson :: PeopleServer UpdateSpecificPerson
updateSpecificPerson name person = liftedUpdate (SetPerson name person) $> person

peopleAPI :: PeopleServer PeopleAPI
peopleAPI =
  peopleIndex :<|>
  specificPerson :<|>
  updateSpecificPerson

app :: AcidState PeopleDb -> Application
app db = serveReader @PeopleAPI Proxy db peopleAPI
