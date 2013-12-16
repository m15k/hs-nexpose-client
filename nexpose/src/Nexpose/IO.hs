-- License:     MIT
-- Maintainer:  Montez Fitzpatrick
-- Stability:   experimental
module Nexpose.IO where

import Network.Curl

sendRecv uri req = withCurlDo $ do
  -- initialize curl and set options
  curl <- initialize
  setopts curl (opts req)
  
  -- POST request
  res <- do_curl_ curl uri [] :: IO CurlResponse

  return res

opts :: String -> [CurlOption]
opts req = 
 [ CurlHttpHeaders ["Content-type: text/xml"]
 , CurlPort (3780 :: Long) -- FIXME
 , CurlUserAgent "Haskell Nexpose Client"
 , CurlPostFields [req]
 , CurlPost True
 , CurlNoBody False
 ] 
