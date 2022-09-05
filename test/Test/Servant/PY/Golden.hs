{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Servant.PY.Golden where

import Control.Applicative (liftA2)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Data.Maybe (fromMaybe)
import Data.Proxy
import GHC.IO.Exception (ExitCode (..))
import Network.Wai.Handler.Warp (defaultSettings, run, setPort, testWithApplication, testWithApplicationSettings, setLogger)
import Servant
import Servant.PY
import System.Directory (copyFile)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.Golden (goldenVsFileDiff, goldenVsStringDiff)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Program (testProgram)
import Debug.Trace

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
          writeTypedPythonForAPI (Proxy @Api1) requests "test/out/api.py"
          -- thread <- forkIO $ run 8000 (serve (Proxy @Api1) addServer)
          -- threadDelay 10000
          -- pure thread
      )
      (\_ -> pure ())
      $ \_ -> do
        testCase "query_server" $
          -- testWithApplicationSettings (setLogger (\req status size -> print req) $ setPort 8000 defaultSettings) (pure $ serve (Proxy @Api1) addServer) $ \port -> do
          -- testWithApplicationSettings (setPort 8000 defaultSettings) (pure $ serve (Proxy @Api1) addServer) $ \port -> do
          testWithApplicationSettings defaultSettings (pure $ serve (Proxy @Api1) addServer) $ \port -> do
            print port
            -- threadDelay 100000000000000000
            (exitCode, _, stdErr) <- readProcessWithExitCode "python3" ["test/out/test_server.py"] ""
            case exitCode of
              ExitSuccess -> pure ()
              ExitFailure _ -> error stdErr
  ]

-- runSettings :: Settings -> Application -> IO ()
-- runSettings set app = withSocketsDo $
--     UnliftIO.bracket
--         (bindPortTCP (settingsPort set) (settingsHost set))
--         close
--         (\socket -> do
--             setSocketCloseOnExec socket
--             runSettingsSocket set socket app)
addServer :: Server Api1
addServer =
  (\n1 n2 -> pure . fromMaybe (-1) $ liftA2 (+) n1 n2)
    :<|> (\(n1, n2) -> pure $ n1 + n2)
    :<|> (\n1 n2 -> pure $ n1 + n2)
    :<|> (\n1May n2 -> pure $ maybe (-1) (+ n2) n1May)
    :<|> (\n1 (fromMaybe 0 -> n2) (fromMaybe 0 -> n3) n4 -> pure $ n1 + n2 + n3 + n4)

differ :: FilePath -> FilePath -> [String]
differ ref new = ["diff", "-u", ref, new]
