{-# LANGUAGE OverloadedStrings #-}

module Etc ( getUsername, getPassword, withEcho, imap ) where

import Control.Exception
import Control.Monad

import qualified Data.ByteString as BS

import System.IO

getUsername :: IO BS.ByteString
getUsername = do
  getInput "username: " True

getPassword :: IO BS.ByteString
getPassword = do
  getInput "password: " False

getInput :: BS.ByteString -> Bool -> IO BS.ByteString
getInput promptStr echoBool = do
  BS.putStr promptStr
  hFlush stdout
  input <- withEcho echoBool BS.getLine
  unless echoBool $ putStrLn ""
  return input

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

imap _ [] = []
imap f xs = imap' f xs 0

imap' f (x : xs) c = f c x : imap' f xs (c + 1)