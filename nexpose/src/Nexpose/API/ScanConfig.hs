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
module Nexpose.API.ScanConfig where
--  ( 
  -- * Main Datatypes
  -- * Picklers
--  ) where

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree
import Data.ByteString.Lazy.Char8 (pack, unpack)

import Nexpose.URI

data ScanConfig = ScanConfig
  { scanCfg_configId :: !String
  , scanCfg_name :: !String
  , scanCfg_templateId :: !String
  , scanCfg_engineId :: Maybe String
  , scanCfg_configVersion :: !String
  , scanCfg_schedules :: Maybe Schedules
  } deriving (Eq, Show)

newtype Schedules = Schedules { schedule :: Maybe [Schedule] } deriving (Eq, Show)

data Schedule = Schedule
  { schedule_enabled :: !String
  , schedule_type :: Maybe String
  , schedule_interval :: !String
  , schedule_start :: !String
  , schedule_maxDuration :: Maybe String
  , schedule_notValidAfter :: Maybe String
  } deriving (Eq, Show)

xpScanConfig :: PU [UNode String] ScanConfig
xpScanConfig =
  xpWrap (\((cfgid, name, tempid, engid, cfgvers), sched) ->
                ScanConfig cfgid name tempid engid cfgvers sched,
          \(ScanConfig cfgid name tempid engid cfgvers sched) ->
                ((cfgid, name, tempid, engid, cfgvers), sched)) $
  xpElem "ScanConfig"
    (xp5Tuple
          (xpAttr "configID" xpText0)
          (xpAttr "name" xpText0)
          (xpAttr "templateID" xpText0)
          (xpOption (xpAttr "engineID" xpText0))
          (xpAttr "configVersion" xpText0))
    (xpOption xpSchedules)

xpSchedules :: PU [UNode String] Schedules
xpSchedules =
  xpWrap ( Schedules,
          \(Schedules xs) -> (xs)) $
  xpElemNodes "Schedules" (xpOption $ xpList0 xpSchedule)

xpSchedule :: PU [UNode String] Schedule
xpSchedule =
  xpWrap (\((en,ty,invl,start,maxd,notval)) -> Schedule en ty invl start maxd notval,
          \(Schedule en ty invl start maxd notval) -> (en,ty,invl,start,maxd,notval)) $
  xpElemAttrs "Schedule"
    (xp6Tuple
          (xpAttr "enabled" xpText0)
          (xpOption (xpAttr "type" xpText0))
          (xpAttr "interval" xpText0)
          (xpAttr "start" xpText0)
          (xpOption (xpAttr "maxDuration" xpText0))
          (xpOption (xpAttr "notValidAfter" xpText0)))
