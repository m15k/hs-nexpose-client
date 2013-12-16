{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 
-- 
-- License:     MIT
-- Maintainer:  Montez Fitzpatrick
-- Stability:   experimental
-- APIVersion:  1.1
--
-- <desc> 
--
module Nexpose.API.SiteSummary where
--  ( 
  -- * Main Datatypes
  -- * Picklers
--  ) where

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree
import Data.ByteString.Lazy.Char8 (pack, unpack)

import Nexpose.URI

data SiteSummary = SiteSummary
  { siteSummary_id :: !String
  , siteSummary_name :: !String
  , siteSummary_desc :: Maybe String
  , siteSummary_riskfactor :: !String
  , siteSummary_riskscore :: !String
  } deriving (Eq, Show)

xpSiteSummary ::  PU [UNode String] SiteSummary
xpSiteSummary =
  xpWrap (\((i,n,d,rf,rs)) -> SiteSummary i n d rf rs,
          \(SiteSummary i n d rf rs) -> (i,n,d,rf,rs)) $
  xpElemAttrs "SiteSummary" 
    (xp5Tuple
          (xpAttr "id" xpText0)
          (xpAttr "name" xpText0)
          (xpOption (xpAttr "description" xpText0))
          (xpAttr "riskfactor" xpText0)
          (xpAttr "riskscore" xpText0))
-- OKAY