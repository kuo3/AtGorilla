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
        | length args == 2 && head args == "cc" -> cc GhcMode $ args !! 1
        | length args == 1 && head args == "pwc" -> pwc
        | length args == 1 && head args == "login" -> login
        | length args == 1 && head args == "logout" -> logout
        | length args == 2 && head args == "test" -> test $ args !! 1
        | length args == 2 && head args == "testw" -> testw $ args !! 1
        | length args == 2 && head args == "submit" -> submit $ args !! 1
        | length args == 1 && head args == "status" -> status
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
      ]
