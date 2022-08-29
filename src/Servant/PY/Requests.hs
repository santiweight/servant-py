{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.PY.Requests where

import Control.Lens ((^.))
import Control.Monad (foldM)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Proxy
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import qualified Language.Python.Common as Py
import Parcel
import Parcel.Python.Gen
import Servant.Foreign
import Servant.PY.Internal
import Data.Bifunctor

-- | Generate python functions that use the requests library.
--   Uses 'defCommonGeneratorOptions' for the generator options.
requests :: PythonGenerator
requests reqs = defPyImports <> mkDecls reqs <> "\n" <> T.intercalate "\n" (map requestsWithDef reqs)

mkDecls :: [PythonRequest] -> Text
mkDecls reqs =
  let tydReqs = flip mapMaybe reqs $ \case
        PythonRequest tyReq -> Just tyReq

      segmentAllTys = segmentTypeAllTys . unSegment
      segmentTypeAllTys = \case
        Static _ -> []
        Cap arg -> [arg ^. argType]
      pathAllTys = (>>= segmentAllTys)
      urlAllTys (url :: Url Parcel.ParcelRepr) = pathAllTys (url ^. path) <> [] :: [Parcel.ParcelRepr]
      allTys =
        Set.fromList $
          traceShowId $
            tydReqs >>= \tydReq ->
              urlAllTys (tydReq ^. reqUrl)
                <> maybeToList (tydReq ^. reqBody)
                <> maybeToList (tydReq ^. reqReturnType)
      modul = pyMToModule $ foldM (\a b -> (<> a) <$> b) [] $ mapMaybe mkDecl (Set.toList allTys)
   in Parcel.prettyModule modul

-- | Generate python functions that use the requests library.
--   Lets you specify your own 'CommonGeneratorOptions'.
requestsWith :: CommonGeneratorOptions -> [PythonRequest] -> Text
requestsWith opts reqs = T.intercalate "\n" (map (generatePyRequestWith opts) reqs)

-- | python codegen using requests with default options
requestsWithDef :: PythonRequest -> Text
requestsWithDef = generatePyRequestWith defCommonGeneratorOptions

defPyImports :: Text
defPyImports =
  T.unlines
    [ "from urllib import parse",
      "", -- Separate stdlib from 3rd-party imports
      "import requests"
    ]

stmtExpr :: Py.Expr () -> Py.Statement ()
stmtExpr e = Py.StmtExpr e ()

-- | python codegen with requests
generatePyRequestWith :: CommonGeneratorOptions -> PythonRequest -> Text
generatePyRequestWith opts req =
  tshow $
    Py.pretty $
      Parcel.fun (functionName opts req) params (v returnTyStr) $
        concat
          [ [v "url" =: v (makePyUrl opts req)],
            maybeToList $ stmtExpr . v <$> paramDef,
            maybeToList $ stmtExpr . v <$> headerDef,
            [stmtExpr (v requestBuilder @ v <$> ("url" : reqCallArgs))]
          ]
          <> functionReturn (returnMode opts)
  where
    --   <> docStringMarker
    --   <> buildDocString req opts returnVal
    --   <> docStringMarker

    returnTyStr = case req of
      PythonRequest tydReq -> maybe "None" (tyToPyTy . Parcel.parcelToTy) (tydReq ^. reqReturnType)
    mkParam name optTy = maybe (param name) (param' name . v . tyToPyTy) optTy
    params =
      mconcat
        [ uncurry mkParam <$> captures req,
          uncurry mkParam <$> qparams,
          maybeToList (param' (requestBody opts) . v . tyToPyTy . parcelToTy <$> getBodyTy req),
          uncurry param' . bimap (toValidFunctionName . (<>) "header") (v . tyToPyTy) <$> headers
        ]
    headers = retrieveHeaders req
    qparams = paramNamesAndTys req
    method = T.toLower $ getMethod req
    reqCallArgs = remainingReqCall

    remainingReqCall = map snd . filter fst $ zip bools strings
      where
        bools = [not . null $ headers, not . null $ qparams, hasBody req]
        strings = ["headers=headers", "params=params", "json=data"]
    paramDef = case qparams of
      [] -> Nothing
      qparams' -> Just $ "params = " <> toPyDict (fst <$> qparams') <> "\n"
    headerDef = case headers of
      [] -> Nothing
      headers' -> Just $ "headers = " <> getHeaderDict req <> "\n"
    requestBuilder = "resp = requests." <> method
    docStringMarker = "\"\"\"\n"
    returnVal = case returnMode opts of
      DangerMode -> "JSON response from the endpoint"
      RawResponse -> "response (requests.Response) from issuing the request"

functionReturn :: ReturnStyle -> [Py.Statement ()]
functionReturn DangerMode =
  [Py.StmtExpr (v "resp.raise_for_status()") (), Py.StmtExpr (v "return resp.json()") ()]
functionReturn RawResponse =
  [Py.StmtExpr (v "return resp") ()]
