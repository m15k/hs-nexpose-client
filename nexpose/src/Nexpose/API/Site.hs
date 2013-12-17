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
module Nexpose.API.Site where
--  ( 
  -- * Main Datatypes
  -- * Picklers
--  ) where

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree
import Text.XML.Expat.PickleExtended
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.List (isInfixOf)

import Nexpose.URI
import Nexpose.API.ScanConfig

data Site = Site
  { site_id :: !String -- ^ Use (-1) to create a new Site
  , site_name :: !String
  , site_desc :: Maybe String
  , site_riskfactor :: !String
  , site_isdynamic :: !String
  , site_hosts :: !Hosts
  , site_exHosts :: Maybe ExcludedHosts
  , site_credentials :: !Credentials
  , site_alerting :: !Alerting
  , site_users :: Maybe Users
  , site_scanConfig :: !ScanConfig
  } deriving (Eq, Show)

newtype Hosts = Hosts {hostList :: [Host]} deriving (Eq, Show)

-- | Should be /or/ which will probably be handled by smart
--   constructor
data Host =  Host { host :: String }
          | Range
            { range_from :: !String
            , range_to :: Maybe String
            } deriving (Eq, Show)

--data Host = Host
--  { host_range :: Range 
--  | host_host  :: String
--  } deriving (Eq, Show)
--
newtype ExcludedHosts = ExcludedHosts {exhost :: [Host]} deriving (Eq, Show)

-- | Should be /or/ which will probably be handled by smart
--   constructor
-- data ExcludedHost = ExcludedHost Range | ExcludedHost String deriving (Eq, Show)
--  { exHost_range :: Range 
--  , exHost_host  :: String
--  } deriving (Eq, Show)

--data Range = Range
--  { range_from :: !String
--  , range_to :: Maybe String
--  } deriving (Eq, Show)

data Credentials = Credentials
  { creds_adminCreds :: [AdminCredentials]
  } deriving (Eq, Show)

data AdminCredentials = AdminCredentials
--  { acreds_serviceType :: ServiceType
  { acreds_serviceType :: !String
  , acreds_host :: Maybe String
  , acreds_port :: Maybe String
  , acreds_userId :: Maybe String
  , acreds_userPass :: Maybe String
  , acreds_realm :: Maybe String
  -- Element
  , acreds_headers :: !Headers
  , acreds_htmlForms :: !HTMLForms
  , acreds_pemKey :: !PEMKey
  } deriving (Eq, Show)

data Headers = Headers
  { headers_soft403 :: Maybe String
  , headers_webapproot :: Maybe String
  , headers_header :: [Header]
  } deriving (Eq, Show)

data Header = Header
  { hdr_name :: !String
  , hdr_value :: Maybe String
  } deriving (Eq, Show)

data HTMLForms = HTMLForms
  { hForms_parentPage :: Maybe String
  , hForms_soft403 :: Maybe String
  , hForms_webapproot :: Maybe String
  , hForms_HTMLForm :: [HTMLForm]
  } deriving (Eq, Show)

data HTMLForm = HTMLForm
  { hForm_name :: Maybe String
  , hForm_action :: !String
  , hForm_method :: Maybe String
  , hForm_enctype :: Maybe String
  , hForm_Field :: Maybe [Field]
  } deriving (Eq, Show)

data Field = Field
  { field_name :: Maybe String
  , field_value :: Maybe String
  , field_type :: Maybe String
  , field_dynamic :: Maybe String
  , field_checked :: Maybe String
  -- content
  , field_data :: !String
  } deriving (Eq, Show)

data PEMKey = PEMKey
  { pemkey_data :: !String
  } deriving (Eq, Show)

data Alerting = Alerting
  { alerting_Alert :: Maybe [Alert]
  } deriving (Eq, Show)

data Alert = Alert
  { alert_name :: !String
  , alert_enabled :: !String
  , alert_maxAlerts :: !String
  , alert_scanFilter :: Maybe ScanFilter
  , alert_vulnFilter :: Maybe VulnFilter
  , alert_smtp :: Maybe SMTPAlert
  , alert_snmp :: Maybe SNMPAlert
  , alert_syslog :: Maybe SyslogAlert
  } deriving (Eq, Show)

data ScanFilter = ScanFilter
  { scanf_scanStart :: !String
  , scanf_scanStop  :: !String
  , scanf_scanFailed  :: !String
  , scanf_scanPaused  :: !String
  , scanf_scanResumed  :: !String
  , scanf_data  :: !String
  } deriving (Eq, Show)

data VulnFilter = VulnFilter
  { vulnf_severityThreshold :: !String
  , vulnf_confirmed :: !String
  , vulnf_unconfirmed :: !String
  } deriving (Eq, Show)

data SMTPAlert = SMTPAlert
  { smtpA_sender :: !String
  , smtpA_server :: !String
  , smtpA_port :: !String
  , smtpA_limitText :: !String
  , smtpA_recipient :: [Recipient]
  } deriving (Eq, Show)

newtype Recipient = Recipient { recipient :: String } deriving (Eq, Show)

data SNMPAlert = SNMPAlert
  { snmpA_community :: !String
  , snmpA_server :: !String
  , snmpA_port :: !String
  } deriving (Eq, Show)

data SyslogAlert = SyslogAlert
  { syslogA_server :: !String
  , syslogA_port :: !String
  } deriving (Eq, Show)

-- Users should probably be it's own DTD.
newtype Users = Users {user :: [User]} deriving (Eq, Show)
newtype User = User {user_Id :: String} deriving (Eq, Show)

xpSite ::  PU [UNode String] Site
xpSite =
  xpWrap (\((i,n,d,rf,dy),(hs,ehs,cs,a,u,sc)) -> Site i n d rf dy hs ehs cs a u sc,
          \(Site i n d rf dy hs ehs cs a u sc) -> ((i,n,d,rf,dy),(hs,ehs,cs,a,u,sc))) $
  xpElem "Site" 
    (xp5Tuple
          (xpAttr "id" xpText0)
          (xpAttr "name" xpText0)
          (xpOption (xpAttr "description" xpText0))
          (xpAttr "riskfactor" xpText0)
          (xpAttr "isDynamic" xpText0))
    (xp6Tuple
          (xpHosts)
          (xpOption xpExcludedHosts)
          (xpCredentials)
          (xpAlerting)
          (xpOption xpUsers)
          (xpScanConfig))
    
xpHosts :: PU [UNode String] Hosts
xpHosts =
  xpWrap ( Hosts,
          \(Hosts h) -> (h)) $
  xpElemNodes "Hosts" (xpList0 xpHost)

xpHost :: PU [UNode String] Host
xpHost =
  xpAlt tag ps
    where
      tag (Host _)    = 0
      tag (Range _ _) = 1
      ps = [  xpWrap ( Host,
                       \(Host h) -> h) $
              xpElemNodes "host"
                  (xpContent xpText0)
           ,    xpWrap ( uncurry Range,
                        \(Range fr to) -> (fr,to)) $
                xpElemAttrs "range"
                  (xpPair
                     (xpAttr "from" xpText0)
                     (xpOption (xpAttr "to" xpText0)))
           ]

--xpHost' :: PU [UNode String] Host
--xpHost' =
--  xpWrap ( Host,
--          \(Host h) -> h) $
--  xpElemNodes "host" (xpContent xpText0)
--
--xpRange :: PU [UNode String] Host
--xpRange =
--  xpWrap ( uncurry Range,
--          \(Range fr to) -> (fr,to)) $
--  xpElemAttrs "range"
--    (xpPair
--       (xpAttr "from" xpText0)
--       (xpOption (xpAttr "to" xpText0)))

xpExcludedHosts :: PU [UNode String] ExcludedHosts
xpExcludedHosts =
  xpWrap ( ExcludedHosts,
          \(ExcludedHosts h) -> (h)) $
--  xpElemNodes "ExcludedHosts" (xpList0 xpExcludedHost)
  xpElemNodes "ExcludedHosts" (xpList0 xpHost)

--xpExcludedHost :: PU [UNode String] ExcludedHost
--xpExcludedHost =
--  xpWrap ( uncurry ExcludedHost,
--          \(ExcludedHost hs) -> (hs)) $
--  xpTryCatch
--          (xpRange)   
--          (xpAttr "Host" xpText0)

xpCredentials :: PU [UNode String] Credentials
xpCredentials =
  xpWrap ( Credentials,
          \(Credentials ac) -> (ac)) $
  xpElemNodes "Credentials" (xpList0 xpAdminCredentials)

xpAdminCredentials :: PU [UNode String] AdminCredentials
xpAdminCredentials =
  xpWrap (\((st,h,p,uid,upwd,realm),(hdr,hfm,pky)) ->
                AdminCredentials st h p uid upwd realm hdr hfm pky,
          \(AdminCredentials st h p uid upwd realm hdr hfm pky) ->
                ((st,h,p,uid,upwd,realm),(hdr,hfm,pky))) $
  xpElem "AdminCredentials"
    (xp6Tuple
          (xpAttr "service" xpText0)
          (xpOption (xpAttr "host" xpText0))
          (xpOption (xpAttr "port" xpText0))
          (xpOption (xpAttr "USERID" xpText0))
          (xpOption (xpAttr "PASSWORD" xpText0))
          (xpOption (xpAttr "realm" xpText0)))
    (xpTriple
          (xpHeaders)
          (xpHTMLForms)
          (xpPEMKey))


xpHeaders :: PU [UNode String] Headers
xpHeaders =
  xpWrap (\((s403, war), hs) ->  Headers s403 war hs,
          \(Headers s403 war hs) -> ((s403, war), hs)) $
  xpElem "Headers"
    (xpPair
          (xpOption (xpAttr "soft403" xpText0))
          (xpOption (xpAttr "webapproot" xpText0)))
    (xpList0 xpHeader)

xpHeader :: PU [UNode String] Header
xpHeader =
  xpWrap ( uncurry Header,
          \(Header n v) -> (n,v)) $
  xpElemAttrs "Header" 
    (xpPair
          (xpAttr "name" xpText0)   
          (xpOption (xpAttr "value" xpText0)))


xpHTMLForms :: PU [UNode String] HTMLForms
xpHTMLForms =
  xpWrap (\((parpg, s403, war), hs) ->  HTMLForms parpg s403 war hs,
          \(HTMLForms parpg s403 war hs) -> ((parpg, s403, war), hs)) $
  xpElem "HTMLForms"
    (xpTriple
          (xpOption (xpAttr "parentpage" xpText0))
          (xpOption (xpAttr "soft403" xpText0))
          (xpOption (xpAttr "webapproot" xpText0)))
    (xpList0 xpHTMLForm)

xpHTMLForm :: PU [UNode String] HTMLForm
xpHTMLForm =
  xpWrap (\((n,act,meth,enct), f) ->  HTMLForm n act meth enct f,
          \(HTMLForm n act meth enct f) -> ((n,act,meth,enct), f)) $
  xpElem "HTMLForm"
    (xp4Tuple
          (xpOption (xpAttr "name" xpText0))
          (xpAttr "action" xpText0)
          (xpOption (xpAttr "method" xpText0))
          (xpOption (xpAttr "enctype" xpText0)))
    (xpOption (xpList0 xpField))

xpField :: PU [UNode String] Field
xpField =
  xpWrap (\((n,v,t,dy,chk), bs) -> Field n v t dy chk bs,
          \(Field n v t dy chk bs) -> ((n,v,t,dy,chk), bs)) $
  xpElem "Field"
    (xp5Tuple
          (xpOption (xpAttr "name" xpText0))
          (xpOption (xpAttr "value" xpText0))
          (xpOption (xpAttr "type" xpText0))
          (xpOption (xpAttr "dynamic" xpText0))
          (xpOption (xpAttr "checked" xpText0)))
    (xpContent xpText0)

xpPEMKey :: PU [UNode String] PEMKey
xpPEMKey =
  xpWrap ( PEMKey,
          \(PEMKey bs) -> (bs)) $
  xpElemNodes "PEMKey" (xpContent xpText0)

-- stopped here at XPAlerting
xpAlerting :: PU [UNode String] Alerting
xpAlerting =
  xpWrap ( Alerting,
          \(Alerting a) -> (a)) $
  xpElemNodes "Alerting" (xpOption (xpList0 xpAlert))

xpAlert :: PU [UNode String] Alert
xpAlert =
  xpWrap (\((n,e,ma),(sf,vf,smtp,snmp,sysl)) -> Alert n e ma sf vf smtp snmp sysl,
          \(Alert n e ma sf vf smtp snmp sysl) -> ((n,e,ma),(sf,vf,smtp,snmp,sysl))) $
  xpElem "Alert"
    (xpTriple
          (xpAttr "name" xpText0)
          (xpAttr "enabled" xpText0)
          (xpAttr "maxAlerts" xpText0))
    (xp5Tuple
          (xpOption xpScanFilter)
          (xpOption xpVulnFilter)
          (xpOption xpSMTPAlert)
          (xpOption xpSNMPAlert)
          (xpOption xpSyslogAlert))


xpScanFilter :: PU [UNode String] ScanFilter
xpScanFilter =
  xpWrap (\((start,stop,failed,paused,resumed),sdata) ->
                 ScanFilter start stop failed paused resumed sdata,
          \(ScanFilter start stop failed paused resumed sdata) ->
                 ((start,stop,failed,paused,resumed),sdata)) $
  xpElem "scanFilter"
    (xp5Tuple
          (xpAttr "scanStart" xpText0)
          (xpAttr "scanStop" xpText0)
          (xpAttr "scanFailed" xpText0)
          (xpAttr "scanPaused" xpText0)
          (xpAttr "scanResumed" xpText0))
    (xpContent xpText0)

xpVulnFilter :: PU [UNode String] VulnFilter 
xpVulnFilter = 
  xpWrap (\((sthres,con,ucon)) -> VulnFilter sthres con ucon,
          \(VulnFilter sthres con ucon) -> ((sthres,con,ucon))) $
  xpElemAttrs "vulnFilter"
    (xpTriple
          (xpAttr "severityThreshold" xpText0)
          (xpAttr "confirmed" xpText0)
          (xpAttr "unconfirmed" xpText0))

xpSMTPAlert :: PU [UNode String] SMTPAlert
xpSMTPAlert = 
  xpWrap (\((sndr,serv,sport,lt),recip) -> SMTPAlert sndr serv sport lt recip,
          \(SMTPAlert sndr serv sport lt recip) -> ((sndr,serv,sport,lt),recip)) $
  xpElem "SMTPAlert"
    (xp4Tuple
          (xpAttr "sender" xpText0)
          (xpAttr "server" xpText0)
          (xpAttr "port" xpText0)
          (xpAttr "limitText" xpText0))
    (xpList0 xpRecipient)

xpRecipient :: PU [UNode String] Recipient
xpRecipient =
  xpWrap ( Recipient,
          \(Recipient ds) -> ds) $
  xpElemNodes "Recipient" (xpContent xpText0)
 
xpSNMPAlert :: PU [UNode String] SNMPAlert
xpSNMPAlert = 
  xpWrap (\((comm,serv,sport)) -> SNMPAlert comm serv sport,
          \(SNMPAlert comm serv sport) -> ((comm,serv,sport))) $
  xpElemAttrs "SNMPAlert"
    (xpTriple
          (xpAttr "community" xpText0)
          (xpAttr "server" xpText0)
          (xpAttr "port" xpText0))

xpSyslogAlert :: PU [UNode String] SyslogAlert
xpSyslogAlert = 
  xpWrap ( uncurry SyslogAlert,
          \(SyslogAlert srv sprt) -> (srv,sprt)) $
  xpElemAttrs "syslogAlert"
    (xpPair
         (xpAttr "server" xpText0)
         (xpAttr "port" xpText0))

xpUsers :: PU [UNode String] Users
xpUsers = 
  xpWrap ( Users,
          \(Users xs) -> xs) $
  xpElemNodes "Users" (xpList0 xpUser)

xpUser :: PU [UNode String] User
xpUser = 
  xpWrap ( User,
          \(User uid) -> uid) $
  xpElemAttrs "User" (xpAttr "id" xpText0)
