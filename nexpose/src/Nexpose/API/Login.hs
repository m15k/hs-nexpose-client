{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Log on to the Security Console and establish a session.
-- 
-- License:     MIT
-- Maintainer:  Montez Fitzpatrick
-- Stability:   experimental
--
--  If no silo-id is specified, the user's silo will be set to the user's default 
--  silo, if it exists. If the silo-id is not specified, and no silos are
--  defined for the user, then the login fails, unless the user is a super-user.
-- 
-- Session Management
module Nexpose.API.Login where

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree
import Network.Curl
import Data.ByteString.Lazy.Char8 (pack, unpack)

import Nexpose.URI

--  <xs:complexType name="loginRequest">
--    <xs:complexContent>
--      <xs:extension base="syncBean">
--        <xs:sequence/>
--        <xs:attribute name="user-id" type="xs:string" use="required"/>
--        <xs:attribute name="password" type="xs:string" use="required"/>
--        <xs:attribute name="silo-id" type="xs:string"/>
--      </xs:extension>
--    </xs:complexContent>
--  </xs:complexType>
data LoginRequest = LoginRequest
  { loginReq_syncId :: Maybe String
  , loginReq_userId :: !String
  , loginReq_password :: !String
  , loginReq_siloId :: Maybe String
  } deriving (Eq, Show)

instance ApiVersion LoginRequest where
  versionURI a = api11

-- need to derive schematype like in Bindings3.hs
-- maybe something like Aeson ToXML / FromXML class
-- then have a login function that would take both the Request and Response as a boundary
-- login :: LoginRequest -> LoginResponse

--  <xs:complexType name="loginResponse">
--    <xs:complexContent>
--      <xs:extension base="response">
--        <xs:sequence/>
--        <xs:attribute name="session-id" type="xs:string" use="required"/>
--      </xs:extension>
--    </xs:complexContent>
--  </xs:complexType> 
data LoginResponse = LoginResponse
  { loginRes_sessId :: Maybe String
  , loginRes_status :: !String
  } deriving (Eq, Show)
 
xpLoginRequest :: PU [UNode String] LoginRequest
xpLoginRequest =
  xpWrap (\((s,u,p,si)) -> LoginRequest s u p si,
          \(LoginRequest s u p si) -> (s,u,p,si)) $
  xpElemAttrs "LoginRequest" 
     (xp4Tuple
           (xpOption (xpAttr "sync-id" xpText0))
           (xpAttr "user-id" xpText0)
           (xpAttr "password" xpText0)
           (xpOption (xpAttr "silo-id" xpText0)))

xpLoginResponse :: PU [UNode String] LoginResponse
xpLoginResponse = 
  xpWrap ( uncurry LoginResponse,
          \(LoginResponse s st) -> (s,st)) $
  xpElemAttrs "LoginResponse" 
    (xpPair
         (xpOption (xpAttr "sync-id" xpText0))
         (xpAttr "session-id" xpText0))


--- REPL Test Functions

-- serialize test should get a workable string
loginReqDoc :: LoginRequest -> String
loginReqDoc l = unpack $ pickleXML (xpRoot $ xpLoginRequest) l

loginResDoc :: String -> LoginResponse
loginResDoc msg = unpickleXML defaultParseOptions (xpRoot xpLoginResponse) (pack msg)

-- logReqTest = LoginRequest Nothing "nxadmin" "passwordy" Nothing
-- logReqDoc = pickleXML (xpRoot $ xpLoginRequest) logReqTest