module Main where

import Control.Exception.Base (bracket)
import Data.Map.Strict (fromList)

import Data.Acid (openLocalState, closeAcidState)
import Network.Wai.Handler.Warp (run)

import Model (PeopleDb(PeopleDb), samplePerson, name)
import Controller (app)

main :: IO ()
main = bracket
  (openLocalState (PeopleDb $ fromList [(name samplePerson, samplePerson)]))
  (closeAcidState)
  (run 3000 . app)
