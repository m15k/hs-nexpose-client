{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Provide the configuration of the site, including associated assets
-- 
-- License:     MIT
-- Maintainer:  Montez Fitzpatrick
-- Stability:   experimental
-- APIVersion:  1.1
--
-- <desc> 
--
module Nexpose.API.SiteConfig where
--  ( 
  -- * Main Datatypes
  -- * Picklers
--  ) where

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree
import Data.ByteString.Lazy.Char8 (pack, unpack)

import Nexpose.URI
import Nexpose.API.Site

data SiteConfigRequest = SiteConfigRequest
  { siteConfigReq_syncId :: Maybe String
  , siteConfigReq_sessionId :: !String
  , siteConfigReq_siteId :: !String -- ^ the ID of the site to retrieve config for
  } deriving (Eq, Show)

instance ApiVersion SiteConfigRequest where
  versionURI a = api11

data SiteConfigResponse = SiteConfigResponse
  { siteConfigRes_success :: !String
 , siteConfigRes_site :: Site
  } deriving (Eq, Show)

xpSiteConfigRequest :: PU [UNode String] SiteConfigRequest
xpSiteConfigRequest =
  xpWrap (\((s,si,site)) -> SiteConfigRequest s si site,
          \(SiteConfigRequest s si site) -> (s,si,site)) $
  xpElemAttrs "SiteConfigRequest" 
     (xpTriple
           (xpOption (xpAttr "sync-id" xpText0))
           (xpAttr "session-id" xpText0)
           (xpAttr "site-id" xpText0))

xpSiteConfigResponse :: PU [UNode String] SiteConfigResponse
xpSiteConfigResponse =
 xpWrap ( uncurry SiteConfigResponse,
         \(SiteConfigResponse s site) -> ((s),site)) $
 xpElem "SiteConfigResponse"
   (xpAttr "success" xpText0)
   (xpSite)
--
-- REPL Test Functions
siteConfigReqDoc :: SiteConfigRequest -> String
siteConfigReqDoc scr = unpack $ pickleXML (xpRoot $ xpSiteConfigRequest) scr

siteConfigResDoc :: String -> SiteConfigResponse
siteConfigResDoc msg = unpickleXML defaultParseOptions (xpRoot xpSiteConfigResponse) (pack msg)
