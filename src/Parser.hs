{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parser where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Data.Functor.Identity
import Data.Maybe
import Data.Tuple.Extra
import Text.Parsec
import Text.Parsec.Char

type Parser = Parsec UTF8ByteString ()

--type Parser = Parsec String ()

searchTokenParser :: Parser String
searchTokenParser = try csrfTokenParser <|> (anyChar >> searchTokenParser)
 where
  csrfTokenParser = do
    string "var csrfToken = \""
    t <- many $ noneOf "\""
    string "\""
    return t

{-
searchSampleParser :: Parser [String]
searchSampleParser = do
  manyTill anyChar (try $ string "<h3>Sample ")
  many sampleParser
 where
  sampleParser :: Parser String
  sampleParser = do
    string "Input" <|> string "Output"
    string " "
    many1 digit
    string "</h3><pre>"
    t <- many $ noneOf "<"
    manyTill anyChar (try $ string "<h3>Sample " <|> (eof >> return ""))
    return t
-}
searchSampleParser :: Parser [String]
searchSampleParser = do
  manyTill anyChar (try $ string "<h3>入力例 " >> many1 digit)
  many sampleParser
 where
  sampleParser :: Parser String
  sampleParser = do
    skipMany $ space <|> newline
    string "</h3>"
    skipMany $ space <|> newline
    string "<pre>"
    skipMany $ space <|> newline
    t <- many $ noneOf "<"
    manyTill anyChar ((try (string "<h3>入力例 ") >> many1 digit) <|> (try (string "<h3>出力例 ") >> many1 digit) <|> try (eof >> return ""))
    return t

data SubmissionsMe = SubmissionsMe
  { submitTime :: String
  , questionName :: String
  , userName :: String
  , language :: String
  , score :: String
  , codeLength :: String
  , result :: String
  , execTime :: String
  , memoryUsage :: String
  }
  deriving (Show)

submissionsMeParser :: Parser [SubmissionsMe]
submissionsMeParser = do
  manyTill anyChar (try (string "<tbody>"))
  skipMany $ space <|> newline
  many $ try tableParser
 where
  tableParser = do
    string "<tr>"
    skipMany $ space <|> newline
    submitTime <- tdParser
    skipMany $ space <|> newline
    questionName <- tdParser
    skipMany $ space <|> newline
    userName <- tdParser
    skipMany $ space <|> newline
    language <- tdParser
    skipMany $ space <|> newline
    score <- tdParser
    skipMany $ space <|> newline
    codeLength <- tdParser
    skipMany $ space <|> newline
    {-
    result <- tdParser
    skipMany $ space <|> newline
    execTime <- tdParser
    skipMany $ space <|> newline
    memoryUsage <- tdParser
    manyTill anyChar (try (string "</tr>"))
    skipMany $ space <|> newline-}
    (result, execTime, memoryUsage) <-
      try td3Parser
        <|> ( do
                result <- tdParser
                execTime <- tdParser
                memoryUsage <- tdParser
                return (result, execTime, memoryUsage)
            )
    manyTill anyChar (try (string "</tr>"))
    skipMany $ space <|> newline
    return
      SubmissionsMe
        { submitTime = submitTime
        , questionName = questionName
        , userName = userName
        , language = language
        , score = score
        , codeLength = codeLength
        , result = result
        , execTime = execTime
        , memoryUsage = memoryUsage
        }

  tdParser = do
    content <- tdContentParser
    manyTill anyChar (try (string "</td>"))
    return content

  td3Parser = do
    content <- try td3ContentParser
    manyTill anyChar (try (string "</td>"))
    return (content, "-", "-")

  tdContentParser =
    try (many1 $ noneOf "<")
      <|> (char '<' >> manyTill anyChar (try (char '>')) >> tdContentParser)

  td3ContentParser =
    try (many1 $ noneOf "<")
      <|> ( try (string "<td colspan='3' class='text-center'><")
              >> manyTill anyChar (try (char '>'))
              >> td3ContentParser
          )

loginParser :: [Char] -> Parser Bool
loginParser name = loginParserP $ "ようこそ、" ++ name ++ " さん。"
 where
  loginParserP msg =
    (try (manyTill anyChar (try (string msg))) >> return True)
      <|> return False

logoutParser :: Parser Bool
logoutParser =
  do
    try (manyTill anyChar (try (string "ログアウトしました。")))
      >> return True
    <|> return False

newtype UTF8ByteString = UTF8ByteString BSL.ByteString

instance Monad m => Stream UTF8ByteString m Char where
  uncons (UTF8ByteString s) = return $ second UTF8ByteString <$> BSLU.uncons s