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
module Nexpose.API.Failure where
--  ( 
  -- * Main Datatypes
  -- * Picklers
--  ) where

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree
import Data.ByteString.Lazy.Char8 (pack, unpack)

import Nexpose.URI

data Failure = Message (Maybe Source) (Maybe Code) !MsgData
             | FailureException (Maybe String) !Failure (Maybe StackTrace)
             deriving (Eq, Show)

xpFailure :: PU [UNode String] Failure
xpFailure =
  xpAlt tag ps
    where
      tag (Message _ _ _) = 0
      tag (FailureException _ _ _) = 1
      ps = [ xpWrap (\((src,code),msg) ->  Message src code msg,
                     \(Message src code msg) -> ((src,code),msg)) $
             xpElem "Message" 
                (xpPair 
                   (xpOption (xpAttr "source" xpText0))
                   (xpOption (xpAttr "code" xpText0)))
                (xpContent xpText0)
           , xpWrap (\((name),(msg,st)) -> FailureException name msg st,
                     \(FailureException name msg st) -> ((name),(msg,st))) $
             xpElem "Exception"
                (xpOption (xpAttr "name" xpText0))
                (xpPair
                   (xpMessage)
                   (xpOption (xpElemNodes "StackTrace" (xpContent xpText0))))
           ]

xpMessage :: PU [UNode String] Failure
xpMessage =
  xpWrap (\((src,code),msg) ->  Message src code msg,
          \(Message src code msg) -> ((src,code),msg)) $
  xpElem "Message" 
    (xpPair 
       (xpOption (xpAttr "source" xpText0))
       (xpOption (xpAttr "code" xpText0)))
    (xpContent xpText0)

type Source = String
type Code = String
type MsgData = String
type StackTrace = String

-- REPL Test Functions

