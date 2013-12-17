{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Delete a site and all associated scan data
-- 
-- License:     MIT
-- Maintainer:  Montez Fitzpatrick
-- Stability:   experimental
-- APIVersion:  1.1
--
-- <desc> 
--
module Nexpose.API.SiteDelete where
--  ( 
  -- * Main Datatypes
  -- * Picklers
--  ) where

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree
import Data.ByteString.Lazy.Char8 (pack, unpack)

import Nexpose.URI

data SiteDeleteRequest = SiteDeleteRequest
  { siteDeleteReq_syncId :: Maybe String
  , siteDeleteReq_sessionId :: !String
  , siteDeleteReq_siteId :: !String
  } deriving (Eq, Show)

instance ApiVersion SiteDeleteRequest where
  versionURI a = api11

data SiteDeleteResponse = SiteDeleteResponse
  { siteDeleteRes_success :: !String
  } deriving (Eq, Show)

xpSiteDeleteRequest :: PU [UNode String] SiteDeleteRequest
xpSiteDeleteRequest =
  xpWrap (\((syd,sess,siteid)) -> SiteDeleteRequest syd sess siteid,
          \(SiteDeleteRequest syd sess siteid) -> ((syd,sess,siteid))) $
  xpElemAttrs "SiteDeleteRequest"
    (xpTriple
          (xpOption (xpAttr "sync-id" xpText0))
          (xpAttr "session-id" xpText0)
          (xpAttr "site-id" xpText0))

xpSiteDeleteResponse :: PU [UNode String] SiteDeleteResponse
xpSiteDeleteResponse =
  xpWrap ( SiteDeleteResponse,
          \(SiteDeleteResponse x) -> x) $
  xpElemAttrs "SiteDeleteResponse" (xpAttr "success" xpText0)

-- REPL Test Functions

