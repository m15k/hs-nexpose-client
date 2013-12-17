{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Save changes to a new or existing site
-- 
-- License:     MIT
-- Maintainer:  Montez Fitzpatrick
-- Stability:   experimental
-- APIVersion:  1.1
--
module Nexpose.API.SiteSave where
--  ( 
  -- * Main Datatypes
  -- * Picklers
--  ) where

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree
import Data.ByteString.Lazy.Char8 (pack, unpack)

import Nexpose.URI
import Nexpose.API.Site

data SiteSaveRequest = SiteSaveRequest
  { siteSaveReq_syncId :: Maybe String
  , siteSaveReq_sessionId :: !String
  , siteSaveReq_site :: !Site
  } deriving (Eq, Show)

instance ApiVersion SiteSaveRequest where
  versionURI a = api11

data SiteSaveResponse = SiteSaveResponse -- TODO: Handle Failure
  { siteSaveRes_success :: !String
  , siteSaveRes_siteId :: !String
  } deriving (Eq, Show)

xpSiteSaveRequest :: PU [UNode String] SiteSaveRequest
xpSiteSaveRequest =
  xpWrap (\((sy,sess),ssite) ->  SiteSaveRequest sy sess ssite,
          \(SiteSaveRequest sy sess ssite) -> ((sy,sess),ssite)) $
  xpElem "SiteSaveRequest"
    (xpPair
        (xpOption (xpAttr "sync-id" xpText0))
        (xpAttr "session-id" xpText0))
    (xpSite)

xpSiteSaveResponse :: PU [UNode String] SiteSaveResponse
xpSiteSaveResponse =
  xpWrap ( uncurry SiteSaveResponse,
          \(SiteSaveResponse succ sid) -> (succ,sid)) $
  xpElemAttrs "SiteSaveResponse"
    (xpPair
        (xpAttr "success" xpText0)
        (xpAttr "site-id" xpText0))

-- REPL Test Functions

