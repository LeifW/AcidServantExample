{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, FlexibleInstances #-}
module API where

import Servant.API
import Servant.Docs
import Model

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
