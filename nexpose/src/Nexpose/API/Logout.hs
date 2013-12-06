{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Log off from the Security Console, freeing the session and all related
--   resources
--
-- License:     MIT
-- Maintainer:  Montez Fitzpatrick
-- Stability:   experimental
--
-- Session Management
module Nexpose.API.Logout where

import Data.ByteString.Lazy.Char8 (pack, unpack)
import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree

import Nexpose.URI

--  <xs:complexType name="LogoutRequest">
--    <xs:complexContent>
--      <xs:extension base="request">
--        <xs:sequence/>
--      </xs:extension>
--    </xs:complexContent>
--  </xs:complexType> 
data LogoutRequest = LogoutRequest
  { logoutReq_syncId :: Maybe String -- ^ used to avoid duplicate requests
  , logoutReq_sessId :: !String
  } deriving (Eq, Show)

instance ApiVersion LogoutRequest where
  versionURI a = api11

--  <xs:complexType name="LogoutResponse">
--    <xs:complexContent>
--      <xs:extension base="response">
--        <xs:sequence/>
--      </xs:extension>
--    </xs:complexContent>
--  </xs:complexType>  
data LogoutResponse = LogoutResponse
  { logoutRes_status :: !String } deriving (Eq, Show)

xpLogoutRequest :: PU [UNode String] LogoutRequest
xpLogoutRequest =
  xpWrap ( uncurry LogoutRequest,
          \(LogoutRequest s si) -> (s,si)) $
  xpElemAttrs "LogoutRequest"
    (xpPair
         (xpOption (xpAttr "sync-id" xpText0))
         (xpAttr "session-id" xpText0))


xpLogoutResponse :: PU [UNode String] LogoutResponse
xpLogoutResponse = 
   xpWrap ( LogoutResponse,
           \(LogoutResponse st) -> (st)) $
   xpElemAttrs "LogoutResponse"
     (xpAttr "success" xpText0)


-- REPL Test Functions

-- serialize
logReqTest sessid = LogoutRequest Nothing sessid

-- end up getting returned a bytestring
logoutReqDoc :: LogoutRequest -> String
logoutReqDoc s = unpack $ pickleXML (xpRoot $ xpLogoutRequest) s

-- msg must be a lazy bytestring
logoutResDoc :: String -> LogoutResponse
logoutResDoc msg = unpickleXML defaultParseOptions (xpRoot xpLogoutResponse) (pack msg)
-- logReqDoc :: LogoutRequest -> [String]
-- logReqDoc = runLA (xshow (arr (pickleDoc xpLogoutRequest)))
