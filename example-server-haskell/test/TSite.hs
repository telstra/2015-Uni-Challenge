{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TSite where  

import Snap.Core
import Snap.Test (RequestBuilder) 
import Snap.Snaplet.Test 
import Snap.Test (get, assertSuccess) 
import Site
import Data.ByteString.Char8 as D8   
import Data.Map as DM 
import Data.Text as DT 
import Test.HUnit  
import           Heist() 


handlePositionPath :: ByteString
handlePositionPath = D8.pack ("/api/position" :: String)  

request :: RequestBuilder IO ()
request = get handlePositionPath (DM.empty :: Params) 

testHandlePosition :: Test 
testHandlePosition = TestCase $ do result <- runHandler (Just "devel") request handlePosition app  
                                   case result of
                                       Left err -> assertFailure $ DT.unpack err 
                                       Right response -> assertSuccess response  
                                   -- Could similarly assertBodyContains bytestring response 
