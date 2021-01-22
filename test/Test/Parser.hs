module Test.Parser where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec

unit_loginParser1 :: IO ()
unit_loginParser1 = do
  logindata <- BSL.readFile "./testData/login.txt"
  case parse (loginParser "name") "error" (UTF8ByteString logindata) of
    Left err -> assertFailure $ "パースエラー:" ++ show err
    Right success -> success @?= True

unit_searchSampleParser1 :: IO ()
unit_searchSampleParser1 = do
  sampledata <- BSL.readFile "./testData/sample.txt"
  case parse searchSampleParser "error" (UTF8ByteString sampledata) of
    Left err -> assertFailure $ "パースエラー:" ++ show err
    Right success ->
      success
        @?= [ "1 2\n"
            , "3\n"
            , "aiueokakikukeko\n"
            , "True\n"
            , "あいうえおかきくけこ\n"
            , "False\n"
            ]
