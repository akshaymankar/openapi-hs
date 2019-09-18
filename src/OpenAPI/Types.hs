{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenAPI.Types where

import Control.Applicative ((<|>))
import Data.Aeson hiding (Encoding)
import Data.Aeson.Types hiding (Encoding)
import Data.Char (toLower)
import qualified Data.HashMap.Strict as HM
import Data.Proxy
import Data.Text hiding (toLower)
import GHC.Generics
import GHC.TypeLits

data OpenAPISpec
  = OpenAPISpec
      { specOpenapi :: Text,
        specInfo :: Info,
        specServers :: Maybe [Server],
        specPaths :: Paths,
        specComponents :: Maybe Components,
        specSecurity :: Maybe [SecurityRequirement],
        specTags :: Maybe [Tag],
        specExternalDocs :: Maybe ExternalDocs,
        specExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "spec" OpenAPISpec

data Info
  = Info
      { infoTitle :: Text,
        infoDescription :: Maybe Text,
        infoTermsOfService :: Maybe Text,
        infoContact :: Maybe Contact,
        infoLicense :: Maybe License,
        infoVersion :: Text,
        infoExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "info" Info

data Contact
  = Contact
      { contactName :: Maybe Text,
        contactUrl :: Maybe Text,
        contactEmail :: Maybe Text,
        contactExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "contact" Contact

data License
  = License
      { licenseName :: Text,
        licenseUrl :: Maybe Text,
        licenseExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "license" License

data Server
  = Server
      { serverUrl :: Text,
        serverDescription :: Maybe Text,
        serverVariables :: Maybe (HM.HashMap Text ServerVariable),
        serverExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "server" Server

data ServerVariable
  = ServerVariable
      { variableEnum :: Maybe [Text],
        variableDefault :: Text,
        variableDescription :: Maybe Text,
        variableExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "variable" ServerVariable

data Components
  = Components
      { componentSchemas :: Maybe (HM.HashMap Text (Referable Schema)),
        componentResponses :: Maybe (HM.HashMap Text (Referable Response)),
        componentParameters :: Maybe (HM.HashMap Text (Referable Parameter)),
        componentExamples :: Maybe (HM.HashMap Text (Referable Example)),
        componentRequestBodies :: Maybe (HM.HashMap Text (Referable RequestBody)),
        componentHeaders :: Maybe (HM.HashMap Text (Referable Header)),
        componentSecuritySchemes :: Maybe (HM.HashMap Text (Referable SecurityScheme)),
        componentLinks :: Maybe (HM.HashMap Text (Referable Link)),
        componentCallbacks :: Maybe (HM.HashMap Text (Referable Callback)),
        componentExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "component" Components

-- Keys have to start with a /
data Paths = Paths (HM.HashMap Text PathItem)
  deriving (Show, Eq, Generic)

instance FromJSON Paths where
  parseJSON = withObject "Paths"
    $ \v -> Paths <$> (sequence $ HM.map parseJSON v)

instance ToJSON Paths where
  toJSON (Paths paths) = toJSON paths

data PathItem
  = PathItem
      { pathRef :: Maybe Text,
        pathSummary :: Maybe Text,
        pathDescription :: Maybe Text,
        pathGet :: Maybe Operation,
        pathPut :: Maybe Operation,
        pathPost :: Maybe Operation,
        pathDelete :: Maybe Operation,
        pathOptions :: Maybe Operation,
        pathHead :: Maybe Operation,
        pathPatch :: Maybe Operation,
        pathTrace :: Maybe Operation,
        pathServers :: Maybe [Server],
        pathParameters :: Maybe (Referable Parameter),
        pathExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "path" PathItem

data Operation
  = Operation
      { operationTags :: Maybe [Text],
        operationSummary :: Maybe Text,
        operationDescription :: Maybe Text,
        operationId :: Maybe Text,
        operationParameters :: Maybe [Referable Parameter],
        operationRequestBody :: Maybe (Referable RequestBody),
        operationResponses :: Responses,
        operationCallbacks :: Maybe (HM.HashMap Text (Referable Callback)),
        operationDeprecated :: Maybe Bool,
        operationSecurity :: Maybe [SecurityRequirement],
        operationServers :: Maybe [Server],
        operationExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)

instance FromJSON Operation where
  parseJSON =
    withObject "Operation"
      $ \v -> do
        parsedOperationId <- v .:? "operationId"
        let jsonOpts = prefixedOmitNothingJSONOptions "operation"
        operationWithoutId <- parseExtensibleJSON jsonOpts (Object v)
        pure $ operationWithoutId {operationId = parsedOperationId}

instance ToJSON Operation where
  toJSON operation =
    let jsonOpts = prefixedOmitNothingJSONOptions "operation"
        withWrongOperationId =
          repackExtensions
            $ genericToJSON jsonOpts operation
     in case withWrongOperationId of
          (Object o) ->
            case HM.lookup "id" o of
              Nothing -> (Object o)
              Just opId ->
                Object
                  $ HM.insert "operationId" opId
                  $ HM.delete "id" o
          _ -> error "impossible: toJSON of operation must be an Object"

data ExternalDocs
  = ExternalDocs
      { externalDocsDescription :: Maybe Text,
        externalDocsUrl :: Text,
        externalDocsExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "spec" ExternalDocs

data Parameter
  = Parameter
      { parameterName :: Text,
        parameterIn :: ParameterIn,
        parameterDescription :: Maybe Text,
        parameterRequired :: Maybe Bool,
        parameterDeprecated :: Maybe Bool,
        parameterAllowEmptyValue :: Maybe Bool,
        parameterStyle :: Maybe Text,
        parameterExplode :: Maybe Bool,
        parameterAllowReserved :: Maybe Bool,
        parameterSchema :: Maybe (Referable Schema),
        parameterExample :: Maybe Value,
        parameterExamples :: Maybe (HM.HashMap Text (Referable Example)),
        parameterContent :: Maybe (HM.HashMap Text MediaType),
        parameterExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "parameter" Parameter

data ParameterIn
  = ParameterInQuery
  | ParameterInHeader
  | ParameterInPath
  | ParameterInCookie
  deriving (Show, Eq, Generic)

instance FromJSON ParameterIn where
  parseJSON (String "cookie") = pure $ ParameterInCookie
  parseJSON (String "header") = pure $ ParameterInHeader
  parseJSON (String "path") = pure $ ParameterInPath
  parseJSON (String "query") = pure $ ParameterInQuery
  parseJSON v = typeMismatch "expected cookie, header, path or query" v

instance ToJSON ParameterIn where
  toJSON ParameterInCookie = String "cookie"
  toJSON ParameterInHeader = String "header"
  toJSON ParameterInPath = String "path"
  toJSON ParameterInQuery = String "query"

data RequestBody
  = RequestBody
      { requestBodyDescription :: Maybe Text,
        requestBodyContent :: HM.HashMap Text MediaType,
        requestBodyRequired :: Maybe Bool
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "requestBody" RequestBody

data MediaType
  = MediaType
      { mediaTypeSchema :: Maybe (Referable Schema),
        mediaTypeExample :: Maybe Value,
        mediaTypeExamples :: Maybe (HM.HashMap Text (Referable Example)),
        mediaTypeEncoding :: Maybe (HM.HashMap Text Encoding),
        mediaTypeExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "mediaType" MediaType

data Encoding
  = Encoding
      { encodingContentType :: Maybe Text,
        encodingHeaders :: Maybe (HM.HashMap Text (Referable Header)),
        encodingStyle :: Maybe Text,
        encodingExplode :: Maybe Bool,
        encodingAllowReserved :: Maybe Bool,
        encodingExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "encoding" Encoding

data Responses
  = Responses
      { responsesDefault :: Maybe (Referable Response),
        responseByStatus :: HM.HashMap Text (Referable Response),
        responsesExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)

instance FromJSON Responses where
  parseJSON =
    withObject "Responses"
      $ \v -> do
        defaultResponses <- v .: "default" <|> pure Nothing
        let exts = Extensions $ extractExtensions v
            byStatusValues = HM.filterWithKey (\k _ -> (not $ "x-" `isPrefixOf` k) && k /= "default") v
        byStatus <- sequence $ HM.map parseJSON byStatusValues
        pure $ Responses defaultResponses byStatus exts

instance ToJSON Responses where
  toJSON Responses {..} =
    let objectWithDefault =
          case toJSON responsesDefault of
            Null -> HM.empty
            v -> HM.singleton "default" v
        objectWithByStatus = HM.map toJSON responseByStatus
        exts = unExtensions responsesExtensions
     in Object $ objectWithDefault `HM.union` objectWithByStatus `HM.union` exts

data Response
  = Response
      { responseDescription :: Text,
        responseHeaders :: Maybe (HM.HashMap Text (Referable Header)),
        responseContent :: Maybe (HM.HashMap Text MediaType),
        responseLinks :: Maybe (HM.HashMap Text (Referable Link)),
        responseExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "response" Response

-- Keys are supposed to be expression
data Callback
  = Callback
      { callback :: HM.HashMap Text PathItem,
        callbackExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)

instance FromJSON Callback where
  parseJSON =
    withObject "Callback"
      $ \v -> do
        let exts = Extensions $ extractExtensions v
            callbackValues = HM.filterWithKey (\k _ -> not $ "x-" `isPrefixOf` k) v
        callbackItems <- sequence $ HM.map parseJSON callbackValues
        pure $ Callback callbackItems exts

instance ToJSON Callback where
  toJSON Callback {..} =
    let callbackMap = HM.map toJSON callback
        Extensions exts = callbackExtensions
     in Object (HM.union callbackMap exts)

data Example
  = Example
      { exampleSummary :: Maybe Text,
        exampleDescription :: Maybe Text,
        exampleValue :: Maybe Value,
        exampleExternalValue :: Maybe Text,
        exampleExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "example" Example

-- Either one of operationId or operationRef are required
-- The `Value` in parameters and request body are actually Any|Expression
data Link
  = Link
      { linkOperationId :: Maybe Text,
        linkOperationRef :: Maybe Text,
        linkParameters :: Maybe (HM.HashMap Text Value),
        linkRequestBody :: Maybe Value,
        linkDescription :: Maybe Text,
        linkServer :: Maybe Server,
        linkExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "link" Link

data Header
  = Header
      { headerDescription :: Maybe Text,
        headerRequired :: Maybe Bool,
        headerDeprecated :: Maybe Bool,
        headerAllowEmptyValue :: Maybe Bool,
        headerStyle :: Maybe Text,
        headerExplode :: Maybe Bool,
        headerAllowReserved :: Maybe Bool,
        headerSchema :: Maybe (Referable Schema),
        headerExample :: Maybe Value,
        headerExamples :: Maybe (HM.HashMap Text (Referable Example)),
        headerContent :: Maybe (HM.HashMap Text MediaType),
        headerExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "header" Header

data Tag
  = Tag
      { tagName :: Text,
        tagDescription :: Maybe Text,
        tagExternalDocs :: Maybe ExternalDocs,
        tagExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "tag" Tag

data Reference = Reference Text
  deriving (Show, Eq, Generic)

data Referable a
  = Refered Reference
  | NotRefered a
  deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (Referable a) where
  parseJSON v =
    (Refered <$> parseJSON v)
      <|> (NotRefered <$> parseJSON v)

instance ToJSON a => ToJSON (Referable a) where
  toJSON (Refered (Reference ref)) = object ["$ref" .= ref]
  toJSON (NotRefered x) = toJSON x

data Schema
  = Schema
      { schemaTitle :: Maybe Text,
        schemaMultipleOf :: Maybe Int,
        schemaMaximum :: Maybe Int,
        schemaExclusiveMaximum :: Maybe Bool,
        schemaMinimum :: Maybe Int,
        schemaExclusiveMinimum :: Maybe Bool,
        schemaMaxLength :: Maybe Int,
        schemaMinLength :: Maybe Int,
        schemaPattern :: Maybe Text,
        schemaMaxItems :: Maybe Int,
        schemaMinItems :: Maybe Int,
        schemaUniqueItems :: Maybe Bool,
        schemaMaxProperties :: Maybe Int,
        schemaMinProperties :: Maybe Int,
        schemaRequired :: Maybe [Text],
        schemaEnum :: Maybe [Value],
        schemaType :: Maybe Text,
        schemaAllOf :: Maybe (Referable Schema),
        schemaOneOf :: Maybe (Referable Schema),
        schemaAnyOf :: Maybe (Referable Schema),
        schemaNot :: Maybe (Referable Schema),
        schemaItems :: Maybe (Referable Schema),
        schemaProperties :: Maybe (HM.HashMap Text (Referable Schema)),
        schemaAdditionalProperties :: Maybe (BoolOr (Referable Schema)), -- Should be Bool or schema
        schemaDescription :: Maybe Text,
        schemaFormat :: Maybe Text,
        schemaDefault :: Maybe Value,
        schemaNullable :: Maybe Bool,
        schemaDiscriminator :: Maybe Discriminator,
        schemaReadOnly :: Maybe Bool,
        schemaWriteOnly :: Maybe Bool,
        schemaXml :: Maybe XML,
        schemaExternalDocs :: Maybe ExternalDocs,
        schemaExample :: Maybe Value,
        schemaDeprecated :: Maybe Bool,
        schemaExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "schema" Schema

data BoolOr a
  = IsBool Bool
  | IsNotBool a
  deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (BoolOr a) where
  parseJSON (Bool b) = pure $ IsBool b
  parseJSON v = IsNotBool <$> parseJSON v

instance ToJSON a => ToJSON (BoolOr a) where
  toJSON (IsBool b) = toJSON b
  toJSON (IsNotBool a) = toJSON a

data Discriminator
  = Discriminator
      { discriminatorPropertyName :: Text,
        discriminatorMapping :: Maybe (HM.HashMap Text Text)
      }
  deriving (Show, Eq, Generic)

instance FromJSON Discriminator where
  parseJSON = genericParseJSON $ prefixedOmitNothingJSONOptions "discriminator"

instance ToJSON Discriminator where
  toJSON = genericToJSON $ prefixedOmitNothingJSONOptions "discriminator"

data XML
  = XML
      { xmlName :: Maybe Text,
        xmlNamespace :: Maybe Text,
        xmlPrefix :: Maybe Text,
        xmlAttribute :: Maybe Bool,
        xmlWrapped :: Maybe Bool,
        xmlExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "xml" XML

-- Use `type` to determine which one of these to parse
data SecurityScheme
  = APIKeySS APIKeySecurityScheme
  | HTTP_SS HTTPSecurityScheme
  | OAuthSS OAuthSecurityScheme
  | OpenIdConnectSS OpenIdConnectSecurityScheme
  deriving (Show, Eq, Generic)

instance FromJSON SecurityScheme where
  parseJSON =
    withObject "SecurityScheme"
      $ \v -> do
        ssType <- v .: "type" :: Parser String
        case ssType of
          "apiKey" -> APIKeySS <$> parseJSON (Object v)
          "http" -> HTTP_SS <$> parseJSON (Object v)
          "oauth2" -> OAuthSS <$> parseJSON (Object v)
          "openIdConnect" -> OpenIdConnectSS <$> parseJSON (Object v)
          invalid -> fail $ "Not a valid security scheme type: " ++ invalid

instance ToJSON SecurityScheme where
  toJSON (APIKeySS scheme) = addType scheme "apiKey"
  toJSON (HTTP_SS scheme) = addType scheme "http"
  toJSON (OAuthSS scheme) = addType scheme "oauth2"
  toJSON (OpenIdConnectSS scheme) = addType scheme "openIdConnect"

addType :: ToJSON a => a -> Text -> Value
addType scheme t =
  case toJSON scheme of
    (Object o) -> Object $ HM.union o (HM.singleton "type" (String t))
    _ -> error "impossible: cannot add type to non object"

data APIKeySecurityScheme
  = APIKeySecurityScheme
      { apiKeyDescription :: Maybe Text,
        apiKeyName :: Text,
        apiKeyIn :: APIKeySecuritySchemeIn,
        apiKeyExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "apiKey" APIKeySecurityScheme

data APIKeySecuritySchemeIn
  = APIKeySecuritySchemeInQuery
  | APIKeySecuritySchemeInHeader
  | APIKeySecuritySchemeInCookie
  deriving (Show, Eq, Generic)

instance FromJSON APIKeySecuritySchemeIn where
  parseJSON (String "cookie") = pure $ APIKeySecuritySchemeInCookie
  parseJSON (String "header") = pure $ APIKeySecuritySchemeInHeader
  parseJSON (String "query") = pure $ APIKeySecuritySchemeInQuery
  parseJSON v = typeMismatch "expected cookie, header or query" v

instance ToJSON APIKeySecuritySchemeIn where
  toJSON APIKeySecuritySchemeInCookie = String "cookie"
  toJSON APIKeySecuritySchemeInHeader = String "header"
  toJSON APIKeySecuritySchemeInQuery = String "query"

data HTTPSecurityScheme
  = HTTPSecurityScheme
      { httpScheme :: Text,
        httpBearerFormat :: Maybe Text,
        httpExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "http" HTTPSecurityScheme

data OAuthSecurityScheme
  = OAuthSecurityScheme
      {oauthFlows :: OAuthFlows}
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "oauth" OAuthSecurityScheme -- Not really extensible

data OAuthFlows
  = OAuthFlows
      { oauthFlowsImplicit :: Maybe OAuthFlow,
        oauthFlowsPassword :: Maybe OAuthFlow,
        oauthFlowsClientCredentials :: Maybe OAuthFlow,
        oauthFlowsAuthorizationCode :: Maybe OAuthFlow,
        oauthFlowsExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "oauthFlows" OAuthFlows

data OAuthFlow
  = OAuthFlow
      { oauthFlowAuthorizationUrl :: Text,
        oauthFlowTokenUrl :: Text,
        oauthFlowRefershUrl :: Maybe Text,
        oauthFlowScopes :: HM.HashMap Text Text,
        oauthFlowExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "oauthFlow" OAuthFlow

data OpenIdConnectSecurityScheme
  = OpenIdConnectSecurityScheme
      {openIdConnectUrl :: Text}
  deriving (Show, Eq, Generic)

instance FromJSON OpenIdConnectSecurityScheme where
  parseJSON = genericParseJSON omitNothingJSONOptions

instance ToJSON OpenIdConnectSecurityScheme where
  toJSON = genericToJSON omitNothingJSONOptions

data SecurityRequirement = SecurityRequirements (HM.HashMap Text [Text])
  deriving (Show, Eq, Generic)

instance FromJSON SecurityRequirement where
  parseJSON = fmap SecurityRequirements . parseJSON

instance ToJSON SecurityRequirement where
  toJSON (SecurityRequirements reqs) = toJSON reqs

data Extensions = Extensions {unExtensions :: HM.HashMap Text Value}
  deriving (Show, Eq, Generic)

instance FromJSON Extensions where
  parseJSON (Object o) = pure $ Extensions o
  parseJSON v = typeMismatch "Object" v

instance ToJSON Extensions where
  toJSON (Extensions m) = Object m

instance FromJSON Reference where
  parseJSON (Object o) = case HM.lookup "$ref" o of
    Just (String ref) -> pure $ Reference ref
    _ -> fail "expected to find $ref in Object"
  parseJSON v = typeMismatch "Object" v

newtype Extensible (prefix :: Symbol) a = Extensible a

instance (Generic a, GToJSON Zero (Rep a), KnownSymbol prefix) => ToJSON (Extensible prefix a) where
  toJSON (Extensible a) =
    repackExtensions $ genericToJSON (prefixedOmitNothingJSONOptions $ (symbolVal (Proxy :: Proxy prefix))) a

instance (Generic a, GFromJSON Zero (Rep a), KnownSymbol prefix) => FromJSON (Extensible prefix a) where
  parseJSON =
    withObject "Extensions"
      $ \v ->
        let opts = prefixedOmitNothingJSONOptions (symbolVal (Proxy :: Proxy prefix))
         in Extensible <$> parseExtensibleJSON opts (Object v)

repackExtensions :: Value -> Value
repackExtensions (Object input) =
  case HM.lookup "extensions" input of
    Just (Object exts) ->
      let withoutExts = HM.delete "extensions" input
       in Object $ HM.union withoutExts exts
    _ -> (Object input)
repackExtensions x = x

omitNothingJSONOptions :: Options
omitNothingJSONOptions =
  defaultOptions {omitNothingFields = True}

prefixedOmitNothingJSONOptions :: String -> Options
prefixedOmitNothingJSONOptions prefix =
  omitNothingJSONOptions {fieldLabelModifier = removePrefix prefix}

removePrefix :: String -> String -> String
removePrefix prefix input = maybe input id $ go prefix input
  where
    go [] (i : is) = Just $ toLower i : is
    go _ [] = Nothing
    go (prefixChar : remainingPrefix) (inputChar : remainingInput) =
      if prefixChar == inputChar
        then go remainingPrefix remainingInput
        else Nothing

parseExtensibleJSON :: 
  (Generic a, GFromJSON Zero (Rep a)) => Options -> Value -> Parser a
parseExtensibleJSON opts = do
  withObject "Extensible"
  $ \v ->
    let exts = extractExtensions v
     in genericParseJSON opts (Object $ HM.insert "extensions" (Object exts) v)

extractExtensions :: HM.HashMap Text Value -> HM.HashMap Text Value
extractExtensions = HM.filterWithKey (\k _ -> "x-" `isPrefixOf` k)
