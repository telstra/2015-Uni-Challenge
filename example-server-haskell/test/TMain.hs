module Main ( 
 main
) where 

import TSite
import Test.Framework
import Test.Framework.Providers.HUnit 

main :: IO ()
main = defaultMain serverTests 

serverTests :: [Test] 
serverTests = [ 
              testGroup "Site handlers" 
               [ 
                 testGroup "Get list of positions" $ hUnitTestToTests testHandlePosition
               ] 
              ] 
