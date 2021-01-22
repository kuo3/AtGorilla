{-# LANGUAGE MultiWayIf #-}

module Main where

import AtGLib
import qualified Control.Exception.Safe as E
import KException
import System.Environment

main :: IO ()
main =
  do
    args <- getArgs
    if
        | length args == 2 && head args == "cc" -> cc $ args !! 1
        | length args == 1 && head args == "login" -> login
        | length args == 1 && head args == "logout" -> logout
        | length args == 2 && head args == "test" -> test $ args !! 1
        | length args == 2 && head args == "testw" -> testw $ args !! 1
        | length args == 2 && head args == "submit" -> submit $ args !! 1
        | length args == 1 && head args == "status" -> status
        | length args == 2 && head args == "archive" -> archive $ args !! 1
        | length args == 2 && head args == "extract" -> extract $ args !! 1
        | otherwise -> showHelp
    `E.catch` \applicationException@(ApplicationException code msg stackmsg) ->
      printMsg applicationException
        `E.catch` \systemException@(SystemException code msg stackmsg) ->
          printStackMsg systemException

showHelp :: IO ()
showHelp =
  putStrLn $
    unlines
      [ ""
      , "使用方法"
      , "・準備(コンテスト名確定):"
      , "$ atg cc    <コンテスト名:コンテストトップページのURLの最終\"/\"以降の文字列 (例:abc188)>"
      , "・ログイン:"
      , "$ atg login"
      , "・ログアウト:"
      , "$ atg logout"
      , "・入力例のテスト:"
      , "$ atg test    <プログラムID:コンテスト問題ページのURLの最終\"_\"以降の文字列 (例:a)>"
      , "・入力例のテスト(watch付き):"
      , "$ atg testw   <プログラムID:コンテスト問題ページのURLの最終\"_\"以降の文字列 (例:a)>"
      , "・提出:"
      , "$ atg submit  <プログラムID:コンテスト問題ページのURLの最終\"_\"以降の文字列 (例:a)>"
      , "・提出ステータスを表示:"
      , "$ atg status"
      , "・保存(Main.hsを指定した名前に変更してコンテストフォルダ内に移動):"
      , "$ atg archive   <プログラムID:コンテスト問題ページのURLの最終\"_\"以降の文字列 (例:a)>"
      , "・持ち出し(コンテストフォルダ内の指定した名前のファイルをMain.hsに移動):"
      , "$ atg extract <プログラムID:コンテスト問題ページのURLの最終\"_\"以降の文字列 (例:a)>"
      ]
