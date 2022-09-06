{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.PY.Requests where

import Control.Lens ((^.))
import Control.Monad (foldM)
import Data.Bifunctor
import Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Python.Common as Py
import Parcel
import Parcel.Python.Gen
import Servant.Foreign
import Servant.PY.Internal

-- | Generate python functions that use the requests library.
--   Uses 'defCommonGeneratorOptions' for the generator options.
requests :: PythonGenerator
requests reqs = mkDecls reqs <> "\n" <> prettyT (genPyClient defCommonGeneratorOptions reqs)

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
          tydReqs >>= \tydReq ->
            urlAllTys (tydReq ^. reqUrl)
              <> maybeToList (tydReq ^. reqBody)
              <> maybeToList (tydReq ^. reqReturnType)
      modul = pyMToModule $ (addImports defPyImports *>) $ foldM (\a b -> (<> a) <$> b) [] $ mapMaybe mkDecl (Set.toList allTys)
   in Parcel.prettyModule modul

-- | Generate python functions that use the requests library.
--   Lets you specify your own 'CommonGeneratorOptions'.
requestsWith :: CommonGeneratorOptions -> [PythonRequest] -> Text
requestsWith opts reqs =
  prettyT $ genPyClient opts reqs

-- | python codegen using requests with default options
requestsWithDef :: PythonRequest -> Py.Statement ()
requestsWithDef = generatePyRequestWith defCommonGeneratorOptions

prettyT :: Py.Pretty a => a -> Text
prettyT = tshow . Py.pretty

defPyImports :: [Py.Statement ()]
defPyImports =
  stmtExpr . v
    <$> [ "from urllib import parse",
          "from typing import *",
          "from dataclasses import dataclass",
          "", -- Separate stdlib from 3rd-party imports
          "import requests"
        ]

stmtExpr :: Py.Expr () -> Py.Statement ()
stmtExpr e = Py.StmtExpr e ()

genPyClient :: CommonGeneratorOptions -> [PythonRequest] -> Py.Statement ()
genPyClient opts reqs =
  let apiFuns = map (generatePyRequestWith opts) reqs
   in dataclass $
        Py.Class
          (ident "Client")
          []
          ( Py.AnnotatedAssign (v $ tyToPyTy $ TKnown TStr) (v "api_base") Nothing () : apiFuns
          )
          ()

-- | python codegen with requests
generatePyRequestWith :: CommonGeneratorOptions -> PythonRequest -> Py.Statement ()
generatePyRequestWith opts req@(PythonRequest tydReq) =
  Parcel.fun (functionName opts req) params (v returnTyStr) $
    concat
      [ [v "url" =: v (makePyUrl req)],
        maybeToList $ (v "params" =:) <$> paramDef,
        maybeToList $ (v "headers" =:) <$> headerDef,
        [stmtExpr (v requestBuilder @= (arg (v "url") : remainingReqCall))]
      ]
      <> functionReturn (returnMode opts)
  where
    returnTyStr = case req of
      PythonRequest tydReq -> maybe "None" (tyToPyTy . Parcel.parcelToTy) (tydReq ^. reqReturnType)
    mkParam name optTy = maybe (param name) (param' name . v . tyToPyTy) optTy
    -- TODO headers need to be converted to strings according to `urllib`
    params =
      mconcat
        [ [param "self"],
          uncurry mkParam <$> captures req,
          uncurry mkParam <$> qparams,
          maybeToList (param' (requestBody opts) . v . tyToPyTy . parcelToTy <$> getBodyTy req),
          uncurry param' . bimap (toValidFunctionName . (<>) "header") (v . tyToPyTy) <$> headers
        ]
    headers = retrieveHeaders req
    qparams = paramNamesAndTys req
    method = T.toLower $ getMethod req

    remainingReqCall =
      mconcat
        [ [arg (v "headers=headers") | not . null $ headers],
          [arg (v "params=params") | not . null $ qparams],
          [ Py.ArgKeyword (ident "json") (encodeExpr (parcelToTy reqBodyRepr) (v "data")) ()
            | reqBodyRepr <- maybeToList (tydReq ^. reqBody)
          ]
        ]
    paramDef = case qparams of
      [] -> Nothing
      qparams' -> Just $ toPyDict (fst <$> qparams')
    headerDef = case headers of
      [] -> Nothing
      headers' -> Just $ getHeaderDict req
    requestBuilder = "resp = requests." <> method

functionReturn :: ReturnStyle -> [Py.Statement ()]
functionReturn DangerMode =
  [Py.StmtExpr (v "resp.raise_for_status()") (), Py.StmtExpr (v "return resp.json()") ()]
functionReturn RawResponse =
  [Py.StmtExpr (v "return resp") ()]
