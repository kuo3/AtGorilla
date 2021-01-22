module KException (
  KException (..),
  SystemException (..),
  ApplicationException (..),
  newSystemException,
  newSystemExceptionStack,
  newApplicationException,
  newApplicationExceptionStack,
) where

import Control.Exception.Safe
import Etc

class KException e where
  stackMsg :: e -> String
  writeMsgInLog :: e -> String -> IO ()
  writeStackMsgInLog :: e -> String -> IO ()
  printMsg :: e -> IO ()
  printStackMsg :: e -> IO ()
  exceptionName :: e -> String
  code :: e -> String
  msg :: e -> String
  stack :: e -> [String]
  newKException :: String -> String -> e
  newKExceptionStack :: KException e2 => String -> String -> e2 -> e
  constructor :: (String -> String -> [String] -> e)
  stackMsg kException =
    unlines $
      imap
        ( \i x ->
            let header =
                  if i == 0
                    then ""
                    else replicate (i - 1) ' ' ++ "┗"
             in header ++ x
        )
        $ stack kException

  writeMsgInLog kException path =
    appendFile path $ msg kException

  writeStackMsgInLog kException path = appendFile path $ stackMsg kException

  printMsg kException = putStrLn $ msg kException

  printStackMsg kException = putStrLn $ stackMsg kException

  newKException code msg = constructor code msg [code ++ " : " ++ msg]

  newKExceptionStack code msg e =
    constructor code msg ((code ++ " : " ++ msg) : stack e)

data SystemException = SystemException String String [String]
  deriving (Eq, Show)

instance KException SystemException where
  exceptionName exception = "SystemException"

  code (SystemException code msg stackmsg) = code

  msg (SystemException code msg stackmsg) = msg

  stack (SystemException code msg stackmsg) = stackmsg

  constructor = SystemException

instance Exception SystemException

data ApplicationException = ApplicationException String String [String]
  deriving (Eq, Show)

instance KException ApplicationException where
  exceptionName exception = "ApplicationException"

  code (ApplicationException code msg stackmsg) = code

  msg (ApplicationException code msg stackmsg) = msg

  stack (ApplicationException code msg stackmsg) = stackmsg

  constructor = ApplicationException

instance Exception ApplicationException

type CodeString = String

type MsgString = String

type StackString = [String]

newSystemException :: CodeString -> MsgString -> SystemException
newSystemException = newKException

newSystemExceptionStack ::
  KException e => CodeString -> MsgString -> e -> SystemException
newSystemExceptionStack = newKExceptionStack

newApplicationException :: CodeString -> MsgString -> ApplicationException
newApplicationException = newKException

newApplicationExceptionStack ::
  KException e => CodeString -> MsgString -> e -> ApplicationException
newApplicationExceptionStack = newKExceptionStack
