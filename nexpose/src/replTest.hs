module ReplTest where

import Network.Curl
import Nexpose.IO
import Nexpose.API.Login

getAns = sendRecv "https://10.10.31.30/api/1.1/xml"

lr = LoginRequest Nothing "nxadmin" "Hu77icane" Nothing

-- loginNX = do
  -- x <- getAns (loginReqDoc lr)
  -- loginResDoc (respBody x)
  
