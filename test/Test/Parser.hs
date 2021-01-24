module Test.Parser where

import           Codec.Binary.UTF8.String
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Parser
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec

unit_loginParser1 :: IO ()
unit_loginParser1 = do
  logindata <- BSL.readFile "./testData/login.txt"
  case parse (loginParser "name") "error" (UTF8ByteString logindata) of
    Left err      -> assertFailure $ "パースエラー:" ++ show err
    Right success -> success @?= True

unit_searchSampleParser1 :: IO ()
unit_searchSampleParser1 = do
  sampledata <- BSL.readFile "./testData/sample1.txt"
  case parse searchSampleParser "error" (UTF8ByteString sampledata) of
    Left err      -> assertFailure $ "パースエラー:" ++ show err
    Right success -> success
      @?= [ ("1 2\n", "3\n")
          , ("aiueokakikukeko\n", "True\n")
          , ("あいうえおかきくけこ\n", "False\n")]

unit_searchSampleParser2 :: IO ()
unit_searchSampleParser2 = do
  sampledata <- BSL.readFile "./testData/sample2.txt"
  case parse searchSampleParser "error" (UTF8ByteString sampledata) of
    Left err      -> assertFailure $ "パースエラー:" ++ show err
    Right success -> success
      @?= [ ("1 2\n", "3\n")
          , ("aiueokakikukeko\n", "True\n")
          , ("あいうえおかきくけこ\n", "False\n")]

unit_searchSampleParser3 = do
  sampledata <- BSL.readFile "./testData/sample3.txt"
  case parse searchSampleParser "error" (UTF8ByteString sampledata) of
    Left err      -> assertFailure $ "パースエラー:" ++ show err
    Right success -> success
      @?= [ (unlines ["2", "AND", "OR", ""], unlines ["5", ""])
          , ( unlines ["5", "OR", "OR", "OR", "OR", "OR", ""]
            , unlines ["63", ""])]