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
import Data.HashMap.Strict (HashMap)
import Data.Proxy
import Data.Text hiding (toLower)
import GHC.Generics
import GHC.TypeLits

-- | This is the root document object of the OpenAPI document.
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

-- | The object provides metadata about the API. The metadata MAY be used by the clients if needed, and MAY be presented in editing or documentation generation tools for convenience.
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

-- | Contact information for the exposed API.
data Contact
  = Contact
      { contactName :: Maybe Text,
        contactUrl :: Maybe Text,
        contactEmail :: Maybe Text,
        contactExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "contact" Contact

-- | License information for the exposed API.
data License
  = License
      { licenseName :: Text,
        licenseUrl :: Maybe Text,
        licenseExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "license" License

-- | An object representing a Server.
data Server
  = Server
      { serverUrl :: Text,
        serverDescription :: Maybe Text,
        serverVariables :: Maybe (HashMap Text ServerVariable),
        serverExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "server" Server

-- | An object representing a Server Variable for server URL template substitution.
data ServerVariable
  = ServerVariable
      { variableEnum :: Maybe [Text],
        variableDefault :: Text,
        variableDescription :: Maybe Text,
        variableExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "variable" ServerVariable

-- | Holds a set of reusable objects for different aspects of the OAS. All objects defined within the components object will have no effect on the API unless they are explicitly referenced from properties outside the components object.
data Components
  = Components
      { componentSchemas :: Maybe (HashMap Text (Referable Schema)),
        componentResponses :: Maybe (HashMap Text (Referable Response)),
        componentParameters :: Maybe (HashMap Text (Referable Parameter)),
        componentExamples :: Maybe (HashMap Text (Referable Example)),
        componentRequestBodies :: Maybe (HashMap Text (Referable RequestBody)),
        componentHeaders :: Maybe (HashMap Text (Referable Header)),
        componentSecuritySchemes :: Maybe (HashMap Text (Referable SecurityScheme)),
        componentLinks :: Maybe (HashMap Text (Referable Link)),
        componentCallbacks :: Maybe (HashMap Text (Referable Callback)),
        componentExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "component" Components

-- | Holds the relative paths to the individual endpoints and their operations. The path is appended to the URL from the Server Object in order to construct the full URL. The Paths MAY be empty, due to ACL constraints.
--
-- Keys must start with a /, but it is not validated yet.
data Paths = Paths (HashMap Text PathItem)
  deriving (Show, Eq, Generic)

instance FromJSON Paths where
  parseJSON = withObject "Paths"
    $ \v -> Paths <$> (sequence $ HM.map parseJSON v)

instance ToJSON Paths where
  toJSON (Paths paths) = toJSON paths

-- | Describes the operations available on a single path. A Path Item MAY be empty, due to ACL constraints. The path itself is still exposed to the documentation viewer but they will not know which operations and parameters are available.
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

-- | Describes a single API operation on a path.
data Operation
  = Operation
      { operationTags :: Maybe [Text],
        operationSummary :: Maybe Text,
        operationDescription :: Maybe Text,
        operationId :: Maybe Text,
        operationParameters :: Maybe [Referable Parameter],
        operationRequestBody :: Maybe (Referable RequestBody),
        operationResponses :: Responses,
        operationCallbacks :: Maybe (HashMap Text (Referable Callback)),
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

-- | Allows referencing an external resource for extended documentation.
data ExternalDocs
  = ExternalDocs
      { externalDocsDescription :: Maybe Text,
        externalDocsUrl :: Text,
        externalDocsExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "spec" ExternalDocs

-- | Describes a single operation parameter.
--
-- A unique parameter is defined by a combination of a name and location.
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
        parameterExamples :: Maybe (HashMap Text (Referable Example)),
        parameterContent :: Maybe (HashMap Text MediaType),
        parameterExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "parameter" Parameter

-- | Denotes location of a parameter
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

-- | Describes a single request body.
data RequestBody
  = RequestBody
      { requestBodyDescription :: Maybe Text,
        requestBodyContent :: HashMap Text MediaType,
        requestBodyRequired :: Maybe Bool
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "requestBody" RequestBody

-- | Each Media Type Object provides schema and examples for the media type identified by its key.
data MediaType
  = MediaType
      { mediaTypeSchema :: Maybe (Referable Schema),
        mediaTypeExample :: Maybe Value,
        mediaTypeExamples :: Maybe (HashMap Text (Referable Example)),
        mediaTypeEncoding :: Maybe (HashMap Text Encoding),
        mediaTypeExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "mediaType" MediaType

-- | A single encoding definition applied to a single schema property.
data Encoding
  = Encoding
      { encodingContentType :: Maybe Text,
        encodingHeaders :: Maybe (HashMap Text (Referable Header)),
        encodingStyle :: Maybe Text,
        encodingExplode :: Maybe Bool,
        encodingAllowReserved :: Maybe Bool,
        encodingExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "encoding" Encoding

-- | A container for the expected responses of an operation. The container maps a HTTP response code to the expected response.
--
-- The documentation is not necessarily expected to cover all possible HTTP response codes because they may not be known in advance. However, documentation is expected to cover a successful operation response and any known errors.
data Responses
  = Responses
      { responsesDefault :: Maybe (Referable Response),
        -- ^ The default MAY be used as a default response object for all HTTP codes that are not covered individually by the specification.
        responseByStatus :: HashMap Text (Referable Response),
        -- ^ The Responses Object MUST contain at least one response code, and it SHOULD be the response for a successful operation call.
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

-- | Describes a single response from an API Operation, including design-time, static links to operations based on the response.
data Response
  = Response
      { responseDescription :: Text,
        responseHeaders :: Maybe (HashMap Text (Referable Header)),
        responseContent :: Maybe (HashMap Text MediaType),
        responseLinks :: Maybe (HashMap Text (Referable Link)),
        responseExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "response" Response

-- | A map of possible out-of band callbacks related to the parent operation. Each value in the map is a Path Item Object that describes a set of requests that may be initiated by the API provider and the expected responses. The key value used to identify the callback object is an expression, evaluated at runtime, that identifies a URL to use for the callback operation.
data Callback
  = Callback
      { callback :: HashMap Text PathItem,
        -- ^ Every key is supposed to be an expression as described [here](https://swagger.io/specification/#callbackObject)
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
-- | The Link object represents a possible design-time link for a response. The presence of a link does not guarantee the caller's ability to successfully invoke it, rather it provides a known relationship and traversal mechanism between responses and other operations.
--
-- Unlike dynamic links (i.e. links provided in the response payload), the OAS linking mechanism does not require link information in the runtime response.
--
-- For computing links, and providing instructions to execute them, a runtime expression is used for accessing values in an operation and using them as parameters while invoking the linked operation
data Link
  = Link
      { linkOperationId :: Maybe Text,
        linkOperationRef :: Maybe Text,
        linkParameters :: Maybe (HashMap Text Value),
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
        headerExamples :: Maybe (HashMap Text (Referable Example)),
        headerContent :: Maybe (HashMap Text MediaType),
        headerExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "header" Header

-- | Adds metadata to a single tag that is used by the Operation Object. It is not mandatory to have a Tag Object per tag defined in the Operation Object instances.
data Tag
  = Tag
      { tagName :: Text,
        tagDescription :: Maybe Text,
        tagExternalDocs :: Maybe ExternalDocs,
        tagExtensions :: Extensions
      }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Extensible "tag" Tag

-- | A simple object to allow referencing other components in the specification, internally and externally.
--
-- The Reference Object is defined by [JSON Reference](https://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03) and follows the same structure, behavior and rules.
--
-- For this specification, reference resolution is accomplished as defined by the JSON Reference specification and not by the JSON Schema specification.
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

-- | The Schema Object allows the definition of input and output data types. These types can be objects, but also primitives and arrays. This object is an extended subset of the [JSON Schema Specification Wright Draft 00](http://json-schema.org/).
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
        schemaProperties :: Maybe (HashMap Text (Referable Schema)),
        schemaAdditionalProperties :: Maybe (BoolOr (Referable Schema)),
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

-- | When request bodies or response payloads may be one of a number of different schemas, a discriminator object can be used to aid in serialization, deserialization, and validation. The discriminator is a specific object in a schema which is used to inform the consumer of the specification of an alternative schema based on the value associated with it.
--
-- When using the discriminator, inline schemas will not be considered.
data Discriminator
  = Discriminator
      { discriminatorPropertyName :: Text,
        discriminatorMapping :: Maybe (HashMap Text Text)
      }
  deriving (Show, Eq, Generic)

instance FromJSON Discriminator where
  parseJSON = genericParseJSON $ prefixedOmitNothingJSONOptions "discriminator"

instance ToJSON Discriminator where
  toJSON = genericToJSON $ prefixedOmitNothingJSONOptions "discriminator"

-- | A metadata object that allows for more fine-tuned XML model definitions.
--
-- When using arrays, XML element names are not inferred (for singular/plural forms) and the name property SHOULD be used to add that information. See examples for expected behavior.
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
-- | Defines a security scheme that can be used by the operations. Supported schemes are HTTP authentication, an API key (either as a header, a cookie parameter or as a query parameter), OAuth2's common flows (implicit, password, application and access code) as defined in [RFC6749](https://tools.ietf.org/html/rfc6749), and [OpenID Connect Discovery](https://tools.ietf.org/html/draft-ietf-oauth-discovery-06).
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

-- | Allows configuration of the supported OAuth Flows.
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

-- | Configuration details for a supported OAuth Flow
data OAuthFlow
  = OAuthFlow
      { oauthFlowAuthorizationUrl :: Text,
        oauthFlowTokenUrl :: Text,
        oauthFlowRefershUrl :: Maybe Text,
        oauthFlowScopes :: HashMap Text Text,
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

-- | Lists the required security schemes to execute this operation. The name used for each property MUST correspond to a security scheme declared in the Security Schemes under the Components Object.
--
-- Security Requirement Objects that contain multiple schemes require that all schemes MUST be satisfied for a request to be authorized. This enables support for scenarios where multiple query parameters or HTTP headers are required to convey security information.
--
-- When a list of Security Requirement Objects is defined on the OpenAPI Object or Operation Object, only one of the Security Requirement Objects in the list needs to be satisfied to authorize the request.
data SecurityRequirement = SecurityRequirements (HashMap Text [Text])
  deriving (Show, Eq, Generic)

instance FromJSON SecurityRequirement where
  parseJSON = fmap SecurityRequirements . parseJSON

instance ToJSON SecurityRequirement where
  toJSON (SecurityRequirements reqs) = toJSON reqs

-- | Contains extensions for any extensible object.
--
-- Any keys starting with "x-" is considered to be an extension.
data Extensions = Extensions {unExtensions :: HashMap Text Value}
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

parseExtensibleJSON
  :: (Generic a, GFromJSON Zero (Rep a)) => Options -> Value -> Parser a
parseExtensibleJSON opts = do
  withObject "Extensible"
  $ \v ->
    let exts = extractExtensions v
     in genericParseJSON opts (Object $ HM.insert "extensions" (Object exts) v)

extractExtensions :: HashMap Text Value -> HashMap Text Value
extractExtensions = HM.filterWithKey (\k _ -> "x-" `isPrefixOf` k)
