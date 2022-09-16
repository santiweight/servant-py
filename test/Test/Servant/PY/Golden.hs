{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Test.Servant.PY.Golden where

import Control.Applicative (liftA2)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text (Text)
import Debug.Trace
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode (..))
import Network.Wai.Handler.Warp (defaultSettings, testWithApplicationSettings)
import Parcel (parcelUtilsFile)
import Servant
import Servant.PY
import System.Directory (copyFile)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.Golden (goldenVsFileDiff)
import Test.Tasty.HUnit (testCase)

data Record = Record {n1 :: Int, n2 :: Int}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Newtype = Newtype Text
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data SingleConstr = SingleConstr Text
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data ListNewtype = ListNewtype [Text]
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data EitherIntNewtype = LeftInt Int | RightNewtype Newtype
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

type Api1 =
  "add-param" :> QueryParam "n1" Integer :> QueryParam "n2" Integer :> Get '[JSON] Integer
    :<|> "add-body" :> ReqBody '[JSON] (Integer, Integer) :> Post '[JSON] Integer
    :<|> "add-capture" :> Capture "n1" Integer :> Capture "n2" Integer :> Get '[JSON] Integer
    :<|> "add-header" :> Header "Some-Header" Integer :> ReqBody '[JSON] Integer :> Get '[JSON] Integer
    :<|> "add-all"
      :> Capture "n1" Integer
      :> QueryParam "n2" Integer
      :> Header "n3" Integer
      :> ReqBody '[JSON] Integer
      :> Post '[JSON] Integer
    :<|> "add-map" :> ReqBody '[JSON] (Map Int Int) :> Post '[JSON] Int
    :<|> "add-map-newtype-key" :> ReqBody '[JSON] (Map Newtype Int) :> Post '[JSON] Int
    :<|> "add-map-newtype-list-key" :> ReqBody '[JSON] (Map ListNewtype Int) :> Post '[JSON] Int
    :<|> "add-map-single-constr-key" :> ReqBody '[JSON] (Map SingleConstr Int) :> Post '[JSON] Int
    :<|> "add-map-sumty-key" :> ReqBody '[JSON] (Map EitherIntNewtype Int) :> Post '[JSON] Int
    :<|> "add-record" :> ReqBody '[JSON] Record :> Post '[JSON] Int

tests :: [TestTree]
tests =
  [ testGroup
      "golden"
      [ goldenVsFileDiff
          "api"
          differ
          "test/golden/api.py.expected"
          "test/golden/api.py.tmp"
          $ writeTypedPythonForAPI (Proxy @Api1) requests "test/golden/api.py.tmp"
      ],
    withResource
      ( do
          copyFile "test/resources/test_server.py" "test/out/test_server.py"
          fp <- parcelUtilsFile
          copyFile fp "test/out/parcel_utils.py"
          writeTypedPythonForAPI (Proxy @Api1) requests "test/out/api.py"
      )
      (\_ -> pure ())
      $ \_ -> do
        testCase "query_server" $
          testWithApplicationSettings
            defaultSettings (pure $ do !_ <- (traceM $ BSL.unpack $ encode (Map.singleton (Newtype "foo") (1 :: Int))); serve (Proxy @Api1) addServer)
            $ \port -> do
              (exitCode, stdOut, stdErr) <- readProcessWithExitCode "python3" ["test/out/test_server.py", show port] ""
              traceM stdOut
              case exitCode of
                ExitSuccess -> pure ()
                ExitFailure _ -> error stdErr
  ]

addServer :: Server Api1
addServer =
  (\n1 n2 -> pure . fromMaybe (-1) $ liftA2 (+) n1 n2)
    :<|> (\(n1, n2) -> pure $ n1 + n2)
    :<|> (\n1 n2 -> pure $ n1 + n2)
    :<|> (\n1May n2 -> pure $ maybe (-1) (+ n2) n1May)
    :<|> (\n1 (fromMaybe 0 -> n2) (fromMaybe 0 -> n3) n4 -> pure $ n1 + n2 + n3 + n4)
    :<|> (pure . foldl' (+) 0 . fmap (uncurry (+)) . Map.toList)
    :<|> (pure . sum)
    :<|> (pure . sum)
    :<|> (pure . sum)
    :<|> (pure . sum)
    :<|> (\Record {..} -> pure $ n1 + n2)

differ :: FilePath -> FilePath -> [String]
differ ref new = ["diff", "-u", ref, new]
