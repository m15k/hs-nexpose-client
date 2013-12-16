{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Provide a list of all sites the user is authorized to view or manage.
-- 
-- License:     MIT
-- Maintainer:  Montez Fitzpatrick
-- Stability:   experimental
-- APIVersion:  1.1
--
-- <desc> 
--
module Nexpose.API.SiteListing where
--  ( 
  -- * Main Datatypes
  -- * Picklers
--  ) where

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree
import Data.ByteString.Lazy.Char8 (pack, unpack)

import Nexpose.URI
import Nexpose.API.SiteSummary

data SiteListingRequest = SiteListingRequest
  { siteListingReq_syncid :: Maybe String
  , siteListingReq_sessid :: !String
  } deriving (Eq, Show)

instance ApiVersion SiteListingRequest where
  versionURI a = api11

data SiteListingResponse = SiteListingResponse
  { siteListingRes_status :: !String
  , siteListingRes_sitesum :: [SiteSummary]
  } deriving (Eq, Show)

xpSiteListingRequest ::  PU [UNode String] SiteListingRequest
xpSiteListingRequest =
  xpWrap ( uncurry SiteListingRequest,
          \(SiteListingRequest s st) -> (s,st)) $
  xpElemAttrs "SiteListingRequest"
    (xpPair
         (xpOption (xpAttr "sync-id" xpText0))
         (xpAttr "session-id" xpText0))

xpSiteListingResponse :: PU [UNode String] SiteListingResponse
xpSiteListingResponse =
  xpWrap ( uncurry SiteListingResponse,
          \(SiteListingResponse s st) -> ((s),st)) $
  xpElem "SiteListingResponse"
    (xpAttr "success" xpText0)
    (xpList xpSiteSummary)

-- REPL Test Functions

siteListingReqDoc :: SiteListingRequest -> String
siteListingReqDoc slr = unpack $ pickleXML (xpRoot $ xpSiteListingRequest) slr

siteListingResDoc :: String -> SiteListingResponse
siteListingResDoc msg = unpickleXML defaultParseOptions (xpRoot xpSiteListingResponse) (pack msg)
-- OKAY