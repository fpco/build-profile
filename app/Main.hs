{-# LANGUAGE NamedFieldPuns #-}
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


import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Char
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
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
  duration Double
  package Text
  stanza Text
  module Text
  verb Text
  payload Text
  deriving Show
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
  ((), cmd) <-
    simpleOptions
      "0"
      "build-profile"
      "build-profile"
      (pure ())
      (addCommand
         "generate"
         "Generate a database of the profile"
         generate
         (do configTitle <-
               fmap
                 T.pack
                 (strOption
                    (long "title" <> help "Title for this build" <>
                     metavar "TEXT"))
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
             pure Config {..}))
  cmd
  where
    generate config = do
      runSqlite
        (configSqliteFile config)
        (do runMigration migrateAll
            mapM_
              (\fp ->
                 runConduitRes
                   (lineFileSource fp .|
                    collectMeasurements (configTitle config) .|
                    CL.mapM_ (lift . insert_)))
              (configLogFiles config))

--------------------------------------------------------------------------------
-- Collector of measurements

data State =
  State
    { statePackageNameVer :: !(Maybe PackageNameVer)
    , stateStanzaName :: !(Maybe StanzaName)
    , stateModuleName :: !(Maybe ModuleName)
    , statePackageStart :: !(Maybe UTCTime)
    }

collectMeasurements :: Monad m => Text -> ConduitT Line Measurement m ()
collectMeasurements measurementTitle =
  collect
    State
      { statePackageStart = Nothing
      , stateStanzaName = Nothing
      , stateModuleName = Nothing
      , statePackageNameVer = Nothing
      } .|
  diff Nothing
  where
    collect state = do
      mline <- await
      case mline of
        Nothing -> pure ()
        Just line ->
          case lineVerb line of
            Configuring packageNameVer -> do
              produce state' ""
              collect state'
              where state' =
                      State
                        { statePackageStart = Just (lineTimestamp line)
                        , stateStanzaName = Nothing
                        , stateModuleName = Nothing
                        , statePackageNameVer = Just packageNameVer
                        }
            Registering ->
              case statePackageStart state of
                Nothing -> pure ()
                Just packageStart -> do
                  yield
                    ( lineTimestamp line
                    , Right
                        (Measurement
                           { measurementTitle
                           , measurementTimestamp = packageStart
                           , measurementDuration =
                               realToFrac
                                 (diffUTCTime (lineTimestamp line) packageStart)
                           , measurementPackage =
                               maybe
                                 ""
                                 (T.decodeUtf8 . unPackageNameVer)
                                 (statePackageNameVer state)
                           , measurementStanza = ""
                           , measurementModule = ""
                           , measurementVerb = "build-package"
                           , measurementPayload = ""
                           }))
                  collect
                    State
                      { statePackageNameVer = Nothing
                      , stateStanzaName = Nothing
                      , stateModuleName = Nothing
                      , statePackageStart = Nothing
                      }
            BuildingLibrary ->
              collect
                state
                  { stateStanzaName = Just (StanzaName "library")
                  , stateModuleName = Nothing
                  }
            BuildingStanza stanzaName ->
              collect state {stateStanzaName = Just stanzaName}
            Compiling moduleName -> do
              produce state' ""
              collect state'
              where state' = state {stateModuleName = Just moduleName}
            Linking file -> do
              produce state' (T.pack file)
              collect state'
              where state' = state {stateModuleName = Nothing}
            Unknown payload -> do
              produce state payload
              collect state
          where produce state' payload =
                  yield
                    ( lineTimestamp line
                    , Left
                        (\duration ->
                           Measurement
                             { measurementTitle
                             , measurementTimestamp = lineTimestamp line
                             , measurementDuration = duration
                             , measurementPayload = payload
                             , measurementPackage =
                                 maybe
                                   ""
                                   (T.decodeUtf8 . unPackageNameVer)
                                   (statePackageNameVer state')
                             , measurementStanza =
                                 maybe
                                   ""
                                   (T.decodeUtf8 . unStanzaName)
                                   (stateStanzaName state')
                             , measurementModule =
                                 maybe
                                   ""
                                   (T.decodeUtf8 . unModuleName)
                                   (stateModuleName state')
                             , measurementVerb =
                                 case lineVerb line of
                                   Configuring {} -> "configure"
                                   Registering {} -> "register"
                                   BuildingLibrary -> "build-library"
                                   BuildingStanza {} -> "build-stanza"
                                   Compiling {} -> "compile"
                                   Linking {} -> "link"
                                   Unknown {} -> "unknown"
                             }))
    diff mprevious = do
      mthis <- await
      case mthis of
        Nothing ->
          case mprevious of
            Just (_, Right measurement) -> yield measurement
            _ -> pure ()
        Just (thisTimestamp, _thisMeasurement) ->
          case mprevious of
            Just (previousTimestamp, eitherPreviousMeasurement) -> do
              yield
                (either
                   ($ realToFrac (diffUTCTime thisTimestamp previousTimestamp))
                   id
                   eitherPreviousMeasurement)
              diff mthis
            Nothing -> diff mthis

--------------------------------------------------------------------------------
-- SAX lexer

data Line =
  Line
    { lineTimestamp :: !UTCTime
    , lineVerb :: !Verb
    }
  deriving (Show)

data Verb
  = Configuring PackageNameVer
  | BuildingLibrary
  | BuildingStanza StanzaName
  | Compiling ModuleName
  | Linking FilePath
  | Registering
  | Unknown Text
  deriving (Show)

newtype PackageNameVer =
  PackageNameVer
    { unPackageNameVer :: ByteString
    }
  deriving (Show)

newtype ModuleName =
  ModuleName
    { unModuleName :: ByteString
    }
  deriving (Show)

newtype StanzaName =
  StanzaName
    { unStanzaName :: ByteString
    }
  deriving (Show)

lineFileSource :: MonadResource m => FilePath -> ConduitT () Line m ()
lineFileSource fp = CB.sourceFile fp .| lineSource

lineSource :: MonadIO m => ConduitT ByteString Line m ()
lineSource =
  CB.lines .| debug .| CL.mapM (either error pure . Atto.parseOnly lineParser)
  where
    debug
      | True = CL.map id
      | otherwise = CL.mapM (\x -> liftIO (print x) >> pure x)

lineParser :: Atto.Parser Line
lineParser = do
  timestamp <- Atto.takeWhile (not . isSpace)
  Atto.skipSpace
  line <- case parseTimeM
         False
         defaultTimeLocale
         "%Y-%m-%dT%H:%M:%S%Q"
         (S8.unpack timestamp) of
    Nothing -> fail ("Invalid timestamp: " <> show timestamp)
    Just utctime -> do
      verb <- verbParser
      pure (Line {lineTimestamp = utctime, lineVerb = verb})
  pure line

verbParser :: Atto.Parser Verb
verbParser =
  Atto.choice
    [ configuring
    , buildinglibrary
    , buildingstanza
    , compiling
    , linking
    , registering
    , unknown
    ]
  where
    configuring = do
      _ <- Atto.string "Configuring "
      Configuring <$> packageNameVerParser
    buildinglibrary = do
      Atto.string "Building library"
      pure BuildingLibrary
    buildingstanza = do
      Atto.string "Building "
      Atto.skipWhile (/= '\'')
      Atto.char '\''
      BuildingStanza <$> stanzaNameParser
    compiling = do
      Atto.char '['
      _ <- Atto.takeWhile (/= ']')
      Atto.char ']'
      Atto.string " Compiling "
      Compiling <$> moduleNameParser
    linking = do
      _ <- Atto.string "Linking "
      Linking . S8.unpack . stripEllipsis <$> Atto.takeByteString
    registering = do
      _ <- Atto.string "Registering "
      pure Registering
    unknown = Unknown . T.pack . S8.unpack <$> Atto.takeByteString

packageNameVerParser :: Atto.Parser PackageNameVer
packageNameVerParser =
  (PackageNameVer . stripEllipsis) <$>
  Atto.takeWhile1 (\c -> isAlphaNum c || elem c ("-.":: [Char]))

stanzaNameParser :: Atto.Parser StanzaName
stanzaNameParser =
  (StanzaName . stripEllipsis) <$>
  Atto.takeWhile1 (\c -> isAlphaNum c || elem c ("-_" :: [Char]))

moduleNameParser :: Atto.Parser ModuleName
moduleNameParser =
  (ModuleName . stripEllipsis) <$>
  Atto.takeWhile1 (\c -> isAlphaNum c || isUpper c || elem c ("_'.":: [Char]))

stripEllipsis :: ByteString -> ByteString
stripEllipsis x = fromMaybe x (S8.stripSuffix "..." x <|> S8.stripSuffix ".." x)
