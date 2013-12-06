-- |
-- License:     MIT
-- Maintainer:  Montez Fitzpatrick
-- Stability:   experimental
module Nexpose.URI where

class ApiVersion a where
  versionURI :: a -> String

api11 :: String
api11 = "/api/1.1/xml"

api12 :: String
api12 = "api/1.2/xml"
