{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.PY.Internal
  ( PythonGenerator,
    ReturnStyle (..),
    PythonRequest (..),
    CommonGeneratorOptions (..),
    defCommonGeneratorOptions,
    defaultPyIndent,
    indent,
    Indent,
    indenter,
    makePyUrl,
    segmentToStr,
    capturesToFormatArgs,
    toValidFunctionName,
    functionName,
    toPyHeader,
    retrieveHeaders,
    getHeaderDict,
    toPyDict,
    paramNamesAndTys,
    captures,
    getMethod,
    hasBody,
    withFormattedCaptures,
    buildDocString,
    buildHeaderDict,
    formatBuilder,
    getBodyTy,
  )
where

import Control.Applicative (liftA2)
import Control.Lens hiding (List)
import Data.Bifunctor (Bifunctor (second))
import qualified Data.CharSet as Set
import qualified Data.CharSet.Unicode.Category as Set
import Data.Data
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.TypeLits
import Parcel
import Parcel.Python.Gen
import Servant.Foreign
  ( FunctionName,
    HeaderArg (HeaderArg, ReplaceHeaderArg),
    PathSegment (unPathSegment),
    QueryArg,
    Req,
    Segment (Segment),
    SegmentType (Cap, Static),
    argName,
    argPath,
    argType,
    captureArg,
    headerArg,
    isCapture,
    path,
    queryArgName,
    queryStr,
    reqBody,
    reqFuncName,
    reqHeaders,
    reqMethod,
    reqUrl,
    snakeCase,
    _PathSegment,
  )

-- A 'PythonGenerator' just takes the data found in the API type
-- for each endpoint and generates Python code as Text.
-- There are `NoContent` requests and Text requests with typing information.
type PythonGenerator = [PythonRequest] -> Text

newtype PythonRequest = PythonRequest (Req ParcelRepr)
  deriving (Eq, Show)

-- We'd like to encode at the type-level that indentation
-- is some multiplication of whitespace (sorry: never tabs!)
type Indent = (" " :: Symbol)

indent :: Proxy Indent
indent = Proxy

-- The defaultPyIndent function is 4 spaces.
-- You can create a different indentation width by passing a different Int to indenter.
defaultPyIndent :: Proxy Indent -> Text
defaultPyIndent = indenter 4

-- But you can create alternatives by specializing the `indenter` function
-- to other Ints. Then, to get your indentation, pass `indent` to the created function
indenter :: Int -> Proxy Indent -> Text
indenter width space = mconcat $ width `replicate` (T.pack . symbolVal) space
{-# INLINE indenter #-}

-- Created python Functions can have different return styles
data ReturnStyle
  = DangerMode -- Throw caution to the wind and return JSON
  | RawResponse -- Return response object itself

-- | This structure is used by specific implementations to let you
-- customize the output
data CommonGeneratorOptions = CommonGeneratorOptions
  { -- | function generating function names
    functionNameBuilder :: FunctionName -> Text,
    -- | name used when a user want to send the request body
    -- (to let you redefine it)
    requestBody :: Text,
    -- | a prefix we should add to the Url in the codegen
    urlPrefix :: Text,
    -- | indentation to use for Python codeblocks. Create this function by passing an Int to indenter.
    indentation :: Proxy Indent -> Text,
    -- | whether the generated functions return the raw response or content
    returnMode :: ReturnStyle
  }

-- | Default options.
--
-- @
-- > defCommonGeneratorOptions = CommonGeneratorOptions
-- >   { functionNameBuilder = snakeCase
-- >   , requestBody = "body"
-- >   , urlPrefix = ""
-- >   , indentation = "    "  -- 4 spaces
-- >   , returnMode = DangerMode
-- >   }
-- @
defCommonGeneratorOptions :: CommonGeneratorOptions
defCommonGeneratorOptions =
  CommonGeneratorOptions
    { functionNameBuilder = snakeCase,
      requestBody = "data",
      urlPrefix = "http://localhost:8000",
      indentation = defaultPyIndent,
      returnMode = DangerMode
    }

-- | Attempts to reduce the function name provided to that allowed by @'Foreign'@.
--
-- For valid Python function identifiers see the following:
-- https://docs.python.org/3.2/reference/lexical_analysis.html#identifiers
-- valid start chars: Lu, Ll, Lt, Lm, Lo, Nl, the underscore
-- valid continuation chars: valid start chars <> Mn, Mc, Nd, Pc
toValidFunctionName :: Text -> Text
toValidFunctionName t =
  case T.uncons t of
    Just (x, xs) ->
      setFirstChar x `T.cons` T.filter remainder xs
    Nothing -> "_"
  where
    setFirstChar c = if Set.member c firstLetterOK then c else '_'
    remainder c = Set.member c remainderOK
    firstLetterOK =
      filterBmpChars $
        mconcat
          [ Set.fromDistinctAscList "_",
            Set.lowercaseLetter,
            Set.uppercaseLetter,
            Set.titlecaseLetter,
            Set.modifierLetter,
            Set.otherLetter,
            Set.letterNumber
          ]
    remainderOK =
      firstLetterOK
        <> filterBmpChars
          ( mconcat
              [ Set.nonSpacingMark,
                Set.spacingCombiningMark,
                Set.decimalNumber,
                Set.connectorPunctuation
              ]
          )

functionName :: CommonGeneratorOptions -> PythonRequest -> Text
functionName opts (PythonRequest req) = toValidFunctionName (functionNameBuilder opts $ req ^. reqFuncName)

-- Identifiers can only contain codepoints in the Basic Multilingual Plane
-- that is, codepoints that can be encoded in UTF-16 without a surrogate pair (UCS-2)
-- that is, codepoints that can fit in 16-bits, up to 0xffff (65535)
filterBmpChars :: Set.CharSet -> Set.CharSet
filterBmpChars = Set.filter (< '\65536')

-- This function creates a dict where the keys are string representations of variable
-- names. This is due to the way arguments are passed into the function, and these
-- arguments named params. In other words, [("key", "key")] becomes: {"key": key}
toPyDict :: [Text] -> Text
toPyDict dict
  | null dict = "{}"
  | otherwise = "{" <> T.intercalate "," insides <> "}"
  where
    insides = combiner <$> dict
    combiner a = "\"" <> a <> "\": " <> a

-- We also need to make sure we can retrieve just the param names for function args.
paramNamesAndTys :: PythonRequest -> [(Text, Maybe Ty)]
paramNamesAndTys (PythonRequest req) = map (\arg -> (view (queryArgName . argPath) arg, Just $ parcelToTy $ arg ^. queryArgName . argType)) $ req ^.. reqUrl . queryStr . traverse

-- Request headers are also passed into the function that makes the request, so we make
-- a python dict out of them.
toPyHeader :: HeaderArg f -> Text
toPyHeader (HeaderArg n) =
  toValidFunctionName ("header" <> n ^. argName . _PathSegment)
toPyHeader (ReplaceHeaderArg n p)
  | pn `T.isPrefixOf` p = pv <> " + \"" <> rp <> "\""
  | pn `T.isSuffixOf` p = "\"" <> rp <> "\" + " <> pv
  | pn `T.isInfixOf` p =
      "\"" <> T.replace pn ("\" + " <> pv <> " + \"") p
        <> "\""
  | otherwise = p
  where
    pv = toValidFunctionName ("header" <> n ^. argName . _PathSegment)
    pn = "{" <> n ^. argName . _PathSegment <> "}"
    rp = T.replace pn "" p

buildHeaderDict :: [HeaderArg f] -> Text
buildHeaderDict [] = ""
buildHeaderDict hs = "{" <> headers <> "}"
  where
    headers = T.intercalate ", " $ map headerStr hs
    headerStr h =
      "\"" <> h ^. headerArg . argPath <> "\": "
        <> toPyHeader h

getHeaderDict :: PythonRequest -> Text
getHeaderDict (PythonRequest req) = buildHeaderDict $ req ^. reqHeaders

retrieveHeaders :: PythonRequest -> [(Text, Ty)]
retrieveHeaders (PythonRequest req) = (\arg -> (arg ^. argPath, parcelToTy $ arg ^. argType)) <$> req ^.. reqHeaders . each . headerArg

captures :: PythonRequest -> [(Text, Maybe Ty)]
captures (PythonRequest req) = second Just <$> capturesTy' req

capturesTy' :: Req ParcelRepr -> [(Text, Ty)]
capturesTy' req =
  map (liftA2 (,) (unPathSegment . view argName) (parcelToTy . view argType) . captureArg)
    . filter isCapture
    $ req ^. reqUrl . path

makePyUrl :: CommonGeneratorOptions -> PythonRequest -> Text
makePyUrl opts (PythonRequest req) = "\"" <> url <> "\"" <> withFormattedCaptures pathParts
  where
    url = urlPrefix opts <> "/" <> getSegments pathParts
    pathParts = req ^.. reqUrl . path . traverse

getSegments :: forall f. [Segment f] -> Text
getSegments segments =
  if null segments
    then ""
    else T.intercalate "/" (map segmentToStr segments)

withFormattedCaptures :: [Segment f] -> Text
withFormattedCaptures segments = formattedCaptures (capturesToFormatArgs segments)
  where
    formattedCaptures [] = ""
    formattedCaptures xs =
      ".format(\n"
        <> T.intercalate "," (map formatBuilder xs)
        <> ")"

formatBuilder :: Text -> Text
formatBuilder val = val <> "=parse.quote(str(" <> val <> "))"

segmentToStr :: Segment f -> Text
segmentToStr (Segment (Static s)) = s ^. _PathSegment
segmentToStr (Segment (Cap s)) = "{" <> s ^. argName . _PathSegment <> "}"

capturesToFormatArgs :: [Segment f] -> [Text]
capturesToFormatArgs segments = map getSegment $ filter isCapture segments
  where
    getSegment (Segment (Cap a)) = getCapture a
    getSegment _ = ""
    getCapture s = s ^. argName . _PathSegment

captureArgsWithTypes :: [Segment ParcelRepr] -> [Text]
captureArgsWithTypes segments = map getSegmentArgType (filter isCapture segments)
  where
    getSegmentArgType (Segment (Cap a)) = pathPart a <> " (" <> tyToPyTy (parcelToTy $ a ^. argType) <> ")"
    getSegmentArgType _ = ""
    pathPart s = s ^. argName . _PathSegment

buildDocString :: PythonRequest -> CommonGeneratorOptions -> Text -> Text
buildDocString (PythonRequest req) opts returnVal = buildDocString' req opts args returnVal
  where
    args = captureArgsWithTypes $ req ^.. reqUrl . path . traverse

buildDocString' :: forall f. Req f -> CommonGeneratorOptions -> [Text] -> Text -> Text
buildDocString' req opts args returnVal =
  T.toUpper method <> " /" <> url <> "\n"
    <> includeArgs
    <> "\n\n"
    <> indent'
    <> "Returns:\n"
    <> indent'
    <> indent'
    <> returnVal
  where
    method = decodeUtf8 $ req ^. reqMethod
    url = getSegments $ req ^.. reqUrl . path . traverse
    includeArgs = if null args then "" else argDocs
    argDocs =
      indent' <> "Args:\n"
        <> indent'
        <> indent'
        <> T.intercalate ("\n" <> indent' <> indent') args
    indent' = indentation opts indent

getMethod :: PythonRequest -> Text
getMethod (PythonRequest req) = decodeUtf8 $ req ^. reqMethod

hasBody :: PythonRequest -> Bool
hasBody (PythonRequest req) = isJust (req ^. reqBody)

getBodyTy :: PythonRequest -> Maybe ParcelRepr
getBodyTy (PythonRequest req) = req ^. reqBody
