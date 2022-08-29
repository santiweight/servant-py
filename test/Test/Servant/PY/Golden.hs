{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Servant.PY.Golden where

import Data.Proxy
import Servant.API
import Servant.PY
import Test.Tasty
import Test.Tasty.Golden (goldenVsFileDiff, goldenVsStringDiff)

type Api1 =
  "add-param" :> QueryParam "n1" Integer :> QueryParam "n2" Integer :> Get '[JSON] Integer
    :<|> "add-body" :> ReqBody '[JSON] (Integer, Integer) :> Post '[JSON] Integer
    :<|> "add-capture" :> Capture "n1" Integer :> Capture "n2" Integer :> Get '[JSON] Integer
    :<|> "add-header" :> Header "Some-Header" (Integer, Integer) :> Get '[JSON] Integer
    :<|> "add-all"
      :> Capture "n1" Integer
      :> QueryParam "n2" Integer
      :> Header "n3" Integer
      :> ReqBody '[JSON] Integer
      :> Post '[JSON] Integer

tests :: TestTree
tests =
  goldenVsFileDiff
    "api"
    differ
    "test/golden/api.py.expected"
    "test/golden/api.py.tmp"
    $ writeTypedPythonForAPI (Proxy @Api1) requests "test/golden/api.py.tmp"

differ :: FilePath -> FilePath -> [String]
differ ref new = ["diff", "-u", ref, new]
