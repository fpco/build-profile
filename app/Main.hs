{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Generate a build profile in an sqlite database.

module Main where

import           Control.Monad.Catch
import           Control.Monad.Trans.Resource
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Char
import           Data.Conduit
import           Data.Conduit.Attoparsec
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Options.Applicative
import           Options.Applicative.Simple

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Measurement
  title Text
  timestamp UTCTime
  package Text
  stanza Text
  module Text
  verb Text
  UniqueMeasurementTitle title
|]

data Config =
  Config
    { configTitle :: Text
    , configSqliteFile :: Text
    , configLogFiles :: [FilePath]
    }

-- | Main entry point.
main :: IO ()
main = do
  (config, runCmd) <-
    simpleOptions
      "0"
      "build-profile"
      "build-profile"
      (do configTitle <-
            fmap
              T.pack
              (strOption
                 (long "title" <> help "Title for this build" <> metavar "TEXT"))
          configSqliteFile <-
            fmap
              T.pack
              (strOption
                 (long "sqlite-file" <>
                  help "Filepath to use for sqlite database" <>
                  metavar "PATH" <>
                  value "profile.sqlite3"))
          configLogFiles <-
            some (strArgument (help "Log file path" <> metavar "PATH"))
          pure Config {..})
      (pure ())
  runSqlite (configSqliteFile config) (runMigration migrateAll)

data Line =
  Line
    { lineTimestamp :: !UTCTime
    , lineVerb :: !Verb
    }

data Verb
  = Configuring PackageNameVer
  | BuildingLibrary
  | BuildingStanza StanzaName
  | Compiling ModuleName
  | Linking FilePath
  | Unknown

data Context =
  Context
    { contextPackageNameVar :: !(Maybe PackageNameVer)
    , contextModuleName :: !(Maybe ModuleName)
    }

newtype PackageNameVer = PackageNameVer {unPackageNameVer :: ByteString}
newtype ModuleName = ModuleName {unModuleName :: ByteString}
newtype StanzaName = StanzaName {unStanzaName :: ByteString}

lineFileSource :: FilePath -> ConduitT ByteString Line (ResourceT IO) ()
lineFileSource fp = CB.sourceFile fp .| lineSource

lineSource :: MonadThrow m => ConduitT ByteString Line m ()
lineSource = CB.lines .| conduitParser lineParser .| CL.map snd

lineParser :: Atto.Parser Line
lineParser = do
  timestamp <- Atto.takeWhile (not . isSpace)
  case parseTimeM
         False
         defaultTimeLocale
         "%Y-%m-%dT%H:%M:%S%Q"
         (S8.unpack timestamp) of
    Nothing -> fail ("Invalid timestamp: " <> show timestamp)
    Just utctime ->
      do verb <- verbParser
         pure (Line {lineTimestamp = utctime, lineVerb = verb})

verbParser :: Atto.Parser Verb
verbParser =
  Atto.choice
    [ configuring
    , buildinglibrary
    , buildingstanza
    , compiling
    , linking
    , unknown
    ]
  where
    configuring = do
      Atto.string "Configuring "
      Configuring <$> packageNameVerParser
    buildinglibrary = do
      Atto.string "Building library"
      pure BuildingLibrary
    buildingstanza = do
      Atto.string "Building '"
      BuildingStanza <$> stanzaNameParser
    compiling = do
      Atto.char '['
      _ <- Atto.takeWhile (/= ']')
      Atto.char ']'
      Atto.string " Compiling"
      Compiling <$> moduleNameParser
    linking = do
      Atto.string "Linking "
      Linking . S8.unpack . stripEllipsis <$> Atto.takeByteString
    unknown = pure Unknown

packageNameVerParser =
  (PackageNameVer . stripEllipsis) <$>
  Atto.takeWhile (\c -> isAlphaNum c || elem c ("-.":: [Char]))

stanzaNameParser =
  (StanzaName . stripEllipsis) <$>
  Atto.takeWhile (\c -> isAlphaNum c || elem c ("-" :: [Char]))

moduleNameParser =
  (ModuleName . stripEllipsis) <$>
  Atto.takeWhile (\c -> isAlphaNum c || elem c ("'.":: [Char]))

stripEllipsis x = fromMaybe x (S8.stripSuffix "..." x <|> S8.stripSuffix ".." x)