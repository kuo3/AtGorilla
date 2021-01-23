{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module AtGLib (
  Mode (..),
  cc,
  pwc,
  login,
  logout,
  test,
  testw,
  submit,
  status,
  archive,
  extract,
) where

import Codec.Binary.UTF8.String
import Control.Concurrent
import qualified Control.Exception.Safe as E
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Data.Char
import Data.String
import Data.Time.Calendar
import Data.Time.Clock
import Debug.Trace
import Etc
import KException
import Network.HTTP.Conduit
import Parser
import System.Directory
import System.Exit
import qualified System.INotify as SI
import System.IO
import System.Process
import Text.Parsec
import Text.Parsec.Char

atgFolder = "./.atg"

archiveFolder = "./archive"
currentLink = "./current"
contestFile = atgFolder ++ "/contest.txt"

modeFile = atgFolder ++ "/mode.txt"
loginInfoFile = atgFolder ++ "/login.txt"

mainhsFile = "./app/Main.hs"

mainbkhsFile = "./.atg/Main.bk.hs"

data Mode = GhcMode | StackMode
  deriving (Show, Read, Eq)

cc :: Mode -> String -> IO ()
cc mode arg = do
  createDirectoryIfMissing True atgFolder
  createDirectoryIfMissing True archiveFolder
  createDirectoryIfMissing True $ archiveFolder ++ "/" ++ arg
  writeFile contestFile arg
  writeFile modeFile $show mode

  pe <- doesPathExist currentLink
  if pe
    then do
      pisl <- pathIsSymbolicLink currentLink
      if pisl
        then do
          removeDirectoryLink currentLink
          createFileLink (archiveFolder ++ "/" ++ arg) currentLink
        else E.throw $ newApplicationException "INFO-cc-1" "currentパスにリンク以外のファイルがあります。currentリンクは作成しません。"
    else createFileLink (archiveFolder ++ "/" ++ arg) currentLink
  return ()

pwc :: IO ()
pwc =
  do
    eContestName <- E.try getContestName
    case eContestName of
      Left exception@(ApplicationException code msg stack) ->
        E.throw $ newApplicationExceptionStack "INFO-pc-1" "コンテスト情報を取得できなかったため、実行できません。ccオプションを実行してください。" exception
      Right contestName -> do
        putStrLn $ "現在作業中のコンテストは " ++ contestName ++ " です。"
    return ()

login :: IO ()
login = do
  result <- E.try getLoginInfo
  case result of
    Left (ApplicationException code msg stackmsg) -> do
      newLogin
    Right loginInfo ->
      E.throw $
        newApplicationException
          "INFO-login-1"
          "ログイン情報を記録したファイルが見つかったため、別途ログインをしたい場合は、一度ログアウトしてください。"

logout :: IO ()
logout = do
  manager <- newManager tlsManagerSettings
  (contest, csrfToken, cookieJar) <- preLoginWithCookie manager
  let stringUrl = "https://atcoder.jp/logout?lang=ja"
  let fUrlEncodedBody = urlEncodedBody [("csrf_token", fromString csrfToken)]
  (response, cookieJar) <-
    postAccess
      stringUrl
      fUrlEncodedBody
      cookieJar
      manager
  let body = responseBody response
  case parse logoutParser "レスポンスボディのパースに失敗しました。" (UTF8ByteString body) of
    Left msg -> E.throw $ newSystemException "ERROR-logout-1" $ show msg
    Right success ->
      if success
        then do
          writeFile loginInfoFile ""
          putStrLn "ログアウト処理が完了しました。"
          return ()
        else do
          writeFile loginInfoFile ""
          E.throw $
            newSystemException
              "ERROR-logout-2"
              "ログアウト処理に失敗しました。ログイン情報を破棄しました。"

test :: String -> IO ()
test arg = do
  eMode <- E.try getMode
  case eMode of
    Left exception@(ApplicationException code msg stack) ->
      E.throw $ newApplicationExceptionStack "INFO-test-1" "モード情報を取得できなかったため、実行できません。ccオプションを実行してください。" exception
    Right mode -> do
      eContestName <- E.try getContestName
      case eContestName of
        Left exception@(ApplicationException code msg stack) ->
          E.throw $ newApplicationExceptionStack "INFO-test-2" "コンテスト情報を取得できなかったため、実行できません。ccオプションを実行してください。" exception
        Right contestName -> do
          let watchTarget =
                if mode == GhcMode
                  then archiveFolder ++ "/" ++ contestName ++ "/" ++ arg ++ ".hs"
                  else "./app/Main.hs"
          testP arg mode contestName

testw :: String -> IO ()
testw arg = do
  eMode <- E.try getMode
  case eMode of
    Left exception@(ApplicationException code msg stack) ->
      E.throw $ newApplicationExceptionStack "INFO-testw-1" "モード情報を取得できなかったため、実行できません。ccオプションを実行してください。" exception
    Right mode -> do
      eContestName <- E.try getContestName
      case eContestName of
        Left exception@(ApplicationException code msg stack) ->
          E.throw $ newApplicationExceptionStack "INFO-testw-2" "コンテスト情報を取得できなかったため、実行できません。ccオプションを実行してください。" exception
        Right contestName -> do
          let watchTarget =
                if mode == GhcMode
                  then archiveFolder ++ "/" ++ contestName ++ "/" ++ arg ++ ".hs"
                  else "./app/Main.hs"
          testP arg mode contestName
          inotify <- SI.initINotify
          wd <-
            SI.addWatch
              inotify
              [SI.CloseWrite]
              ( fromString
                  watchTarget
              )
              ( \e -> do
                  threadDelay (5 * 100 * 1000)
                  result <- E.try $ testP arg mode contestName
                  case result of
                    Left exception@(ApplicationException code msg stackmsg) ->
                      printMsg exception
                    Right _ -> return ()
              )
          testwp arg wd inotify watchTarget
 where
  testwp :: String -> SI.WatchDescriptor -> SI.INotify -> String -> IO ()
  testwp arg wd inotify watchTarget = do
    putStrLn $
      watchTarget ++ "ファイルの変更確認中。変更確認されれば"
        ++ arg
        ++ "問題サンプルのテストを行います。\n"
        ++ "[a-z|1-9]エンターキーを押すと問題が切り替わります。\n"
        ++ "エンターキーのみを押すと終了します。"
    input <- getLine
    if
        | input == "" ->
          do
            SI.removeWatch wd
            SI.killINotify inotify
            return ()
        | length input
            == 1
            && ( ( ord (head input)
                    >= ord 'a'
                    && ord (head input)
                    <= ord 'z'
                 )
                  || ( ord (head input)
                        >= ord '1'
                        && ord (head input)
                        <= ord '9'
                     )
               ) ->
          do
            SI.removeWatch wd
            SI.killINotify inotify
            testw input
        | otherwise ->
          do
            testwp arg wd inotify watchTarget

submit :: String -> IO ()
submit arg = do
  eMode <- E.try getMode
  case eMode of
    Left exception@(ApplicationException code msg stack) ->
      E.throw $ newApplicationExceptionStack "INFO-testw-1" "モード情報を取得できなかったため、実行できません。ccオプションを実行してください。" exception
    Right mode -> do
      eContestName <- E.try getContestName
      case eContestName of
        Left exception@(ApplicationException code msg stack) ->
          E.throw $ newApplicationExceptionStack "INFO-testw-2" "コンテスト情報を取得できなかったため、実行できません。ccオプションを実行してください。" exception
        Right contestName -> do
          submitFile <-
            if mode == GhcMode
              then BS.readFile $ archiveFolder ++ "/" ++ contestName ++ "/" ++ arg ++ ".hs"
              else BS.readFile mainhsFile
          manager <- newManager tlsManagerSettings
          (contestName, csrfToken, cookieJar) <- preLoginWithCookie manager
          let url = "https://atcoder.jp/contests/" ++ contestName ++ "/submit"
          let fUrlEncodedBody =
                urlEncodedBody
                  [ ("csrf_token", fromString csrfToken)
                  , ("data.TaskScreenName", fromString $ contestName ++ "_" ++ arg)
                  , ("data.LanguageId", fromString "4027")
                  , ("sourceCode", submitFile)
                  ]
          (response, cookieJar) <- postAccess url fUrlEncodedBody cookieJar manager
          putStrLn "提出しました。"
          return ()

status :: IO ()
status = do
  manager <- newManager tlsManagerSettings
  (contestName, csrfToken, cookieJar) <- preLoginWithCookie manager
  let url = "https://atcoder.jp/contests/" ++ contestName ++ "/submissions/me"
  let fSetQueryString =
        setQueryString [("csrf_token", Just $ fromString csrfToken)]
  (response, cookieJar) <- getAccess url fSetQueryString cookieJar manager
  let body = responseBody response
  case parse submissionsMeParser "レスポンスボディのパースに失敗しました。" (UTF8ByteString body) of
    Left errmsg ->
      E.throw $ newApplicationException "ERR-status-1" $ show errmsg
    Right submissionsMe -> do
      let p = " | "
      putStrLn $
        "       Submit time      "
          ++ p
          ++ "   Question name    "
          ++ p
          ++ "      Language      "
          ++ p
          ++ " Score"
          ++ p
          ++ "Result"
          ++ p
          ++ " Exec time"
      let submissionsMeStr =
            foldl
              ( \acc x ->
                  ( submitTime x
                      ++ p
                      ++ take 20 (questionName x ++ replicate 20 ' ')
                      ++ p
                      ++ take 20 (language x ++ replicate 20 ' ')
                      ++ p
                      ++ (replicate (6 - length (score x)) ' ' ++ score x)
                      ++ p
                      ++ (replicate (6 - length (result x)) ' ' ++ result x)
                      ++ p
                      ++ (replicate (10 - length (execTime x)) ' ' ++ execTime x)
                  ) :
                  acc
              )
              []
              submissionsMe
      mapM_ putStrLn submissionsMeStr
  return ()

archive :: String -> IO ()
archive arg = do
  tGetContestName <- E.try getContestName
  case tGetContestName of
    Left exception@(ApplicationException code msg stack) ->
      putStrLn "コンテスト名の取得に失敗しました。ccオプションを実行してください。"
    Right contestName -> do
      let fromFile = mainhsFile
      let toFile = archiveFolder ++ "/" ++ contestName ++ "/" ++ arg ++ ".hs"
      let bkFile =
            archiveFolder ++ "/" ++ contestName ++ "/" ++ arg ++ ".bk.hs"
      tMoveFile <- E.try $ moveFile fromFile toFile bkFile
      case tMoveFile of
        Left exception@(ApplicationException code msg stack) ->
          E.throw exception
        Right _ -> return ()
  return ()

extract :: String -> IO ()
extract arg = do
  tGetContestName <- E.try getContestName
  case tGetContestName of
    Left exception@(ApplicationException code msg stack) ->
      E.throw $
        newApplicationExceptionStack
          "INFO-archive-1"
          "コンテスト名の取得に失敗しました。ccオプションを実行してください。"
          exception
    Right contestName -> do
      let fromFile = archiveFolder ++ "/" ++ contestName ++ "/" ++ arg ++ ".hs"
      let toFile = mainhsFile
      let bkFile = mainbkhsFile
      tMoveFile <- E.try $ moveFile fromFile toFile bkFile
      case tMoveFile of
        Left exception@(ApplicationException code msg stack) ->
          E.throw exception
        Right _ -> return ()

newLogin :: IO ()
newLogin = do
  username <- getUsername
  password <- getPassword
  manager <- newManager tlsManagerSettings
  request1 <- parseRequest "https://atcoder.jp/login?lang=ja"
  response1 <- httpLbs request1 manager
  let cookieJar1 = responseCookieJar response1
  let body1 = responseBody response1
  case parse searchTokenParser "レスポンスボディのパースに失敗しました。" (UTF8ByteString body1) of
    Left errmsg -> E.throw $ newSystemException "ERR-newlogin-1" $ show errmsg
    Right csrfToken -> do
      let stringUrl = "https://atcoder.jp/login?lang=ja"
      let fUrlEncodedBody =
            urlEncodedBody
              [ ("csrf_token", fromString csrfToken)
              , ("username", username)
              , ("password", password)
              ]
      (response, cookieJar) <-
        postAccess
          stringUrl
          fUrlEncodedBody
          cookieJar1
          manager
      let body2 = responseBody response
      let cookieJar2 = responseCookieJar response
      case parse
        (loginParser $ decode $ BS.unpack username)
        "レスポンスボディのパースに失敗しました。"
        (UTF8ByteString body2) of
        Left errmsg ->
          E.throw $ newSystemException "ERR-newlogin-2" $ show errmsg
        Right success ->
          if success
            then do
              writeFile loginInfoFile $ show cookieJar2
              putStr "ログイン処理に成功しました。"
            else
              E.throw $
                newApplicationException
                  "INFO-newLogin-1"
                  "ログインできませんでした。ユーザ名とパスワードを見直してください。"

getLoginInfo :: IO String
getLoginInfo = do
  fe <- doesFileExist loginInfoFile
  unless fe $
    E.throw $
      newApplicationException
        "INFO-getLoginInfo-1"
        "ログイン情報を記録したファイルが見つかりません。"
  loginInfo <- readFile loginInfoFile
  when (loginInfo == "") $
    E.throw $
      newApplicationException
        "INFO-getLoginInfo-2"
        "ログイン情報を記録したファイルの内容が空です。"
  return loginInfo

testP :: String -> Mode -> String -> IO ()
testP arg mode contestName = do
  let buildCommandStr =
        if mode == GhcMode
          then "ghc -o " ++ archiveFolder ++ "/" ++ contestName ++ "/" ++ arg ++ ".out -O0 " ++ archiveFolder ++ "/" ++ contestName ++ "/" ++ arg ++ ".hs"
          else "stack build --fast"

  processHandle <- runCommand buildCommandStr
  manager <- newManager tlsManagerSettings
  resultPreLogin <- E.try $ preLoginWithCookie manager
  case resultPreLogin of
    Left exception@(ApplicationException code msg stack) ->
      E.throw $
        newApplicationExceptionStack
          "INFO-testP-1"
          "事前ログインに失敗しました。"
          exception
    Right (contestName, csrfToken, cookieJar) -> do
      let url =
            "https://atcoder.jp/contests/"
              ++ contestName
              ++ "/tasks/"
              ++ contestName
              ++ "_"
              ++ arg
      let fSetQueryString =
            setQueryString [("csrf_token", Just $ fromString csrfToken)]
      (response, cookieJar) <- getAccess url fSetQueryString cookieJar manager
      let body = responseBody response
      case parse
        searchSampleParser
        ("ERR-testP-1:レスポンスボディのパースに失敗しました。" :: String)
        (UTF8ByteString body) of
        Left errmsg ->
          --E.throw $ newSystemException "ERR-testP-1" $ show errmsg
          print errmsg
        Right samples -> do
          let tests = div (length samples) 2
          exitCode <- waitForProcess processHandle
          case exitCode of
            ExitFailure exitCode ->
              E.throw $
                newApplicationException
                  "INFO-testP-2"
                  "buildに失敗しました。修正してください。"
            ExitSuccess -> do
              putStrLn $ "running " ++ show tests ++ " tests"
              forM_ [0 .. length samples - 1] $ \i ->
                do
                  let (inData, outData) = samples !! i
                  let iname = atgFolder ++ "/" ++ arg ++ "_" ++ show i ++ "_" ++ "i"
                  let oname = atgFolder ++ "/" ++ arg ++ "_" ++ show i ++ "_" ++ "o"
                  let outFileName = atgFolder ++ "/out.txt"
                  writeFile iname inData
                  writeFile oname outData
                  let runCommandStr =
                        if mode == GhcMode
                          then
                            "./" ++ archiveFolder ++ "/" ++ contestName ++ "/" ++ arg ++ ".out >"
                              ++ outFileName
                              ++ " < "
                              ++ iname
                          else
                            "stack run > "
                              ++ outFileName
                              ++ " < "
                              ++ iname

                  processHandle <-
                    runCommand
                      runCommandStr
                  exitCode <- waitForProcess processHandle
                  case exitCode of
                    ExitSuccess -> return ()
                    ExitFailure exitCode ->
                      putStrLn $ "ExitFailure :ExitCode = " ++ show exitCode

                  act <- readFile outFileName
                  if exCR act == exCR outData
                    then putStrLn $ "入力例" ++ show i ++ "---------> OK"
                    else do
                      putStrLn $ "入力例" ++ show i ++ "---------> NG"
                      putStrLn "入力:"
                      putStr inData
                      putStrLn "期待値:"
                      putStr $exCR outData
                      putStrLn "出力:"
                      putStr $exCR act

  return ()
 where
  exCR str = foldr (\x acc -> if x == '\r' then acc else x : acc) [] str

type StringContest = String

type StringCsrfToken = String

type StringUrl = String

type FUrlEncodedBody = (Request -> Request)

type FSetQueryString = (Request -> Request)

preLoginWithCookie :: Manager -> IO (StringContest, StringCsrfToken, CookieJar)
preLoginWithCookie manager = do
  tGetContestName <- E.try getContestName
  case tGetContestName of
    Left exception@(ApplicationException code msg stack) ->
      E.throw $
        newApplicationExceptionStack
          "INFO-preLogin-1"
          "コンテスト名取得に失敗しました。"
          exception
    Right contestName -> do
      loginInfo <- getLoginInfo
      let cookieJar = read loginInfo
      manager <- newManager tlsManagerSettings
      request1' <-
        parseRequest $ "https://atcoder.jp/contests/" ++ contestName ++ "/"
      let request1 = request1'{cookieJar = Just cookieJar}
      response1 <- httpLbs request1 manager
      let cookieJar1 = responseCookieJar response1
      let body1 = responseBody response1
      case parse searchTokenParser "レスポンスボディのパースに失敗しました。" (UTF8ByteString body1) of
        Left errmsg ->
          E.throw $ newSystemException "ERR-preLogin-1" $ show errmsg
        Right csrfToken -> return (contestName, csrfToken, cookieJar)

postAccess ::
  StringUrl ->
  FUrlEncodedBody ->
  CookieJar ->
  Manager ->
  IO (Response BSL.ByteString, CookieJar)
postAccess accessPage fUrlEncodedBody cookieJar manager = do
  request'' <- parseRequest $ "POST " ++ accessPage
  let request' = fUrlEncodedBody request''
  let request = request'{cookieJar = Just cookieJar}
  response <- httpLbs request manager
  let cookieJar = responseCookieJar response
  return (response, cookieJar)

getAccess ::
  StringUrl ->
  FSetQueryString ->
  CookieJar ->
  Manager ->
  IO (Response BSL.ByteString, CookieJar)
getAccess accessPage fSetQueryString cookieJar manager = do
  request'' <- parseRequest accessPage
  let request' = fSetQueryString request''
  let request = request'{cookieJar = Just cookieJar}
  response <- httpLbs request manager
  let cookieJar = responseCookieJar response
  return (response, cookieJar)

getContestName :: IO String
getContestName = do
  feContestFile <- doesFileExist contestFile
  if not feContestFile
    then
      E.throw $
        newApplicationException
          "INFO-getContest-1"
          "コンテスト情報を記録したファイルが作成されていないため、実行できません。ccオプションを実行してください。"
    else do
      contestName <- readFile contestFile
      if contestName == ""
        then
          E.throw $
            newApplicationException
              "INFO-getContest-2"
              "コンテスト情報を記録したファイルが空のため、実行できません。ccオプションを実行してください。"
        else do
          return contestName

getMode :: IO Mode
getMode = do
  feModeFile <- doesFileExist modeFile
  if not feModeFile
    then
      E.throw $
        newApplicationException
          "INFO-getMode-1"
          "モード情報を記録したファイルが作成されていないため、実行できません。ccオプションを実行してください。"
    else do
      modeStr <- readFile modeFile
      if
          | modeStr == "GhcMode" -> return GhcMode
          | modeStr == "StackMode" -> return StackMode
          | otherwise ->
            E.throw $
              newApplicationException
                "INFO-getMode-2"
                "モード情報を記録したファイルの内容が無効なため、実行できません。ccオプションを実行してください。"

getMainhsBS :: IO BS.ByteString
getMainhsBS = do
  feMainhs <- doesFileExist mainhsFile
  if not feMainhs
    then
      E.throw $
        newApplicationException "INFO-getMainhsBS-1" "app/Main.hsファイルが存在しません。"
    else do
      mainhs <- BS.readFile mainhsFile
      if mainhs == ""
        then
          E.throw $
            newApplicationException
              "INFO-getMainhsBS-2"
              "app/Main.hsファイルが空です。"
        else do
          return mainhs

moveFile :: String -> String -> String -> IO ()
moveFile fromFile toFile bkFile = do
  feFrom <- doesFileExist fromFile
  if not feFrom
    then
      E.throw $
        newApplicationException
          "INFO-moveFile-1"
          "fromファイルが存在しないため、実行できません。プロジェクトの構成を見直してください。"
    else do
      feTo <- doesFileExist toFile
      when feTo $ do
        tohs <- BS.readFile toFile
        BS.writeFile bkFile tohs
      fromhs <- BS.readFile fromFile
      BS.writeFile toFile fromhs
      return ()
