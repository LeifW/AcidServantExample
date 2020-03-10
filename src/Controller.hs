{-# LANGUAGE DataKinds, TypeFamilies, TypeApplications, TypeOperators, FlexibleInstances #-}
module Controller where

import Data.Functor (($>))
import Servant
import Data.Acid (AcidState)

import AcidReader
import ServantReaderT
import Model
import API

type PeopleServer api = ServerT api (ReaderHandler (AcidState PeopleDb))

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
