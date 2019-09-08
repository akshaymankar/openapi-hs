{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenAPI.Types where

import Control.Applicative ((<|>))
import Data.Aeson          hiding (Encoding)
import Data.Aeson.Types    hiding (Encoding)
import Data.Char           (toLower)
import Data.Map
import Data.Text           hiding (toLower)
import GHC.Generics

import qualified Data.HashMap.Strict as HM

data OpenAPISpec = OpenAPISpec { specOpenapi      :: Text
                               , specInfo         :: Info
                               , specServers      :: Maybe [Server]
                               , specPaths        :: Paths
                               , specComponents   :: Maybe Components
                               , specSecurity     :: Maybe [SecurityRequirement]
                               , specTags         :: Maybe [Tag]
                               , specExternalDocs :: Maybe ExternalDocs
                               , specExtensions   :: Extensions
                               }
                 deriving (Show, Eq, Generic)

data Info = Info { infoTitle          :: Text
                 , infoDescription    :: Maybe Text
                 , infoTermsOfService :: Maybe Text
                 , infoContact        :: Maybe Contact
                 , infoLicense        :: Maybe License
                 , infoVersion        :: Text
                 , infoExtensions     :: Extensions
                 }
          deriving (Show, Eq, Generic)

data Contact = Contact { contactName       :: Maybe Text
                       , contactUrl        :: Maybe Text
                       , contactEmail      :: Maybe Text
                       , contactExtensions :: Extensions
                       }
               deriving (Show, Eq, Generic)

data License = License { licenseName       :: Text
                       , licenseUrl        :: Maybe Text
                       , licenseExtensions :: Extensions
                       }
               deriving (Show, Eq, Generic)

data Server = Server { serverUrl         :: Text
                     , serverDescription :: Maybe Text
                     , serverVariables   :: Maybe (Map Text ServerVariable)
                     , serverExtensions  :: Extensions
                     }
            deriving (Show, Eq, Generic)

data ServerVariable = ServerVariable { variableEnum        :: Maybe [Text]
                                     , variableDefault     :: Text
                                     , variableDescription :: Maybe Text
                                     , variableExtensions  :: Extensions
                                     }
                    deriving (Show, Eq, Generic)

data Components =
  Components
  { componentSchemas         :: Maybe (Map Text (Referable Schema))
  , componentResponses       :: Maybe (Map Text (Referable Response))
  , componentParameters      :: Maybe (Map Text (Referable Parameter))
  , componentExamples        :: Maybe (Map Text (Referable Example))
  , componentRequestBodies   :: Maybe (Map Text (Referable RequestBody))
  , componentHeaders         :: Maybe (Map Text (Referable Header))
  , componentSecuritySchemas :: Maybe (Map Text (Referable SecurityScheme))
  , componentLinks           :: Maybe (Map Text (Referable Link))
  , componentCallbacks       :: Maybe (Map Text (Referable Callback))
  , componentExtensions      :: Extensions
  }
  deriving (Show, Eq, Generic)

-- Keys have to start with a /
data Paths = Paths (Map Text PathItem)
           deriving (Show, Eq, Generic)

data PathItem = PathItem
                { pathRef         :: Maybe Text
                , pathSummary     :: Maybe Text
                , pathDescription :: Maybe Text
                , pathGet         :: Maybe Operation
                , pathPut         :: Maybe Operation
                , pathPost        :: Maybe Operation
                , pathDelete      :: Maybe Operation
                , pathOptions     :: Maybe Operation
                , pathHead        :: Maybe Operation
                , pathPatch       :: Maybe Operation
                , pathTrace       :: Maybe Operation
                , pathServers     :: Maybe [Server]
                , pathParameters  :: Maybe (Referable Parameter)
                , pathExtensions  :: Extensions
                }
              deriving (Show, Eq, Generic)

data Operation = Operation
                 { operationTags        :: Maybe [Text]
                 , operationSummary     :: Maybe Text
                 , operationDescription :: Maybe Text
                 , operationId          :: Maybe Text
                 , operationParameters  :: Maybe [Referable Parameter]
                 , operationRequestBody :: Maybe (Referable RequestBody)
                 , operationResponses   :: Responses
                 , operationCallbacks   :: Maybe (Map Text (Referable Callback))
                 , operationDeprecated  :: Maybe Bool
                 , operationSecurity    :: Maybe [SecurityRequirement]
                 , operationServers     :: Maybe [Server]
                 , operationExtensions  :: Extensions
                 }
               deriving (Show, Eq, Generic)

data ExternalDocs = ExternalDocs
                    { externalDocsDescription :: Maybe Text
                    , externalDocsUrl         :: Text
                    , externalDocsExtensions  :: Extensions
                    }
                  deriving (Show, Eq, Generic)

data Parameter =
  Parameter
  { parameterName            :: Text
  , parameterIn              :: ParameterIn
  , parameterDescription     :: Maybe Text
  , parameterRequired        :: Maybe Bool
  , parameterDeprecated      :: Maybe Bool
  , parameterAllowEmptyValue :: Maybe Bool
  , parameterStyle           :: Maybe Text
  , parameterExplode         :: Maybe Bool
  , parameterAllowReserved   :: Maybe Bool
  , parameterSchema          :: Maybe (Referable Schema)
  , parameterExample         :: Maybe Value
  , parameterExamples        :: Maybe (Map Text (Referable Example))
  , parameterContent         :: Maybe (Map Text MediaType)
  , parameterExtensions      :: Extensions
  }
  deriving (Show, Eq, Generic)

data ParameterIn = ParameterInQuery
                 | ParameterInHeader
                 | ParameterInPath
                 | ParameterInCookie
                 deriving (Show, Eq, Generic)

data RequestBody = RequestBody
                   { requestBodyDescription :: Maybe Text
                   , requestBodyContent     :: Map Text MediaType
                   , requestBodyRequired    :: Maybe Bool
                   }
                 deriving (Show, Eq, Generic)

data MediaType = MediaType
                 { mediaTypeSchema     :: Maybe (Referable Schema)
                 , mediaTypeExample    :: Maybe Value
                 , mediaTypeExamples   :: Maybe (Map Text (Referable Example))
                 , mediaTypeEncoding   :: Maybe (Map Text Encoding)
                 , mediaTypeExtensions :: Extensions
                 }
               deriving (Show, Eq, Generic)

data Encoding = Encoding
                { encodingContentType   :: Maybe Text
                , encodingHeaders       :: Maybe (Map Text (Referable Header))
                , encodingStyle         :: Maybe Text
                , encodingExplode       :: Maybe Bool
                , encodingAllowReserved :: Maybe Bool
                , encodingExtensions    :: Extensions
                }
              deriving (Show, Eq, Generic)

data Responses = Responses
                 { responsesDefault    :: Maybe (Referable Response)
                 , responseByStatus    :: HM.HashMap Text (Referable Response)
                 , responsesExtensions :: Extensions
                 }
               deriving (Show, Eq, Generic)

data Response = Response
                { responseDescription :: Text
                , responseHeaders     :: Maybe (Map Text (Referable Header))
                , responseContent     :: Maybe (Map Text MediaType)
                , responseLinks       :: Maybe (Map Text (Referable Link))
                , responseExtensions  :: Extensions
                }
              deriving (Show, Eq, Generic)

-- Keys are supposed to be expression
data Callback = CallBack { callback           :: Map Text PathItem
                         , callbackExtensions :: Extensions
                         }
              deriving (Show, Eq, Generic)

data Example = Example
               { exampleSummary       :: Maybe Text
               , exampleDescription   :: Maybe Text
               , exampleValue         :: Maybe Value
               , exampleExternalValue :: Maybe Text
               , exampleExtensions    :: Extensions
               }
             deriving (Show, Eq, Generic)

-- Either one of operationId or operationRef are required
-- The `Value` in parameters and request body are actually Any|Expression
data Link = Link
            { linkOperationId  :: Maybe Text
            , linkOperationRef :: Maybe Text
            , linkParameters   :: Maybe (Map Text Value)
            , linkRequestBody  :: Maybe Value
            , linkDescription  :: Maybe Text
            , linkServer       :: Maybe Server
            , linkExtensions   :: Extensions
            }
          deriving (Show, Eq, Generic)

data Header = Header
              { headerDescription     :: Maybe Text
              , headerRequired        :: Maybe Bool
              , headerDeprecated      :: Maybe Bool
              , headerAllowEmptyValue :: Maybe Bool
              , headerStyle           :: Maybe Text
              , headerExplode         :: Maybe Bool
              , headerAllowReserved   :: Maybe Bool
              , headerSchema          :: Maybe (Referable Schema)
              , headerExample         :: Maybe Value
              , headerExamples        :: Maybe (Map Text (Referable Example))
              , headerContent         :: Maybe (Map Text MediaType)
              , headerExtensions      :: Extensions
              }
            deriving (Show, Eq, Generic)

data Tag = Tag
           { tagName         :: Text
           , tagDescription  :: Maybe Text
           , tagExternalDocs :: Maybe ExternalDocs
           , tagExtensions   :: Extensions
           }
         deriving (Show, Eq, Generic)

data Reference = Reference Text
               deriving (Show, Eq, Generic)

data Referable a = Refered Reference
                 | NotRefered a
                 deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (Referable a) where
  parseJSON v = (Refered <$> parseJSON v)
                <|> (NotRefered <$> parseJSON v)

data Schema = Schema
              { schemaTitle                :: Maybe Text
              , schemaMultipleOf           :: Maybe Int
              , schemaMaximum              :: Maybe Int
              , schemaExclusiveMaximum     :: Maybe Bool
              , schemaMinimum              :: Maybe Int
              , schemaExclusiveMinimum     :: Maybe Bool
              , schemaMaxLength            :: Maybe Int
              , schemaMinLength            :: Maybe Int
              , schemaPattern              :: Maybe Text
              , schemaMaxItems             :: Maybe Int
              , schemaMinItems             :: Maybe Int
              , schemaUniqueItems          :: Maybe Bool
              , schemaMaxProperties        :: Maybe Int
              , schemaMinProperties        :: Maybe Int
              , schemaRequired             :: Maybe [Text]
              , schemaEnum                 :: Maybe [Value]
              , schemaType                 :: Maybe Text
              , schemaAllOf                :: Maybe (Referable Schema)
              , schemaOneOf                :: Maybe (Referable Schema)
              , schemaAnyOf                :: Maybe (Referable Schema)
              , schemaNot                  :: Maybe (Referable Schema)
              , schemaItems                :: Maybe (Referable Schema)
              , schemaProperties           :: Maybe (Map Text (Referable Schema))
              , schemaAdditionalProperties :: Maybe (BoolOr (Referable Schema)) -- Should be Bool or schema
              , schemaDescription          :: Maybe Text
              , schemaFormat               :: Maybe Text
              , schemaDefault              :: Maybe Value
              , schemaNullable             :: Maybe Bool
              , schemaDiscriminator        :: Maybe Discriminator
              , schemaReadOnly             :: Maybe Bool
              , schemaWriteOnly            :: Maybe Bool
              , schemaXml                  :: Maybe XML
              , schemaExternalDocs         :: Maybe ExternalDocs
              , schemaExample              :: Maybe Value
              , schemaDeprecated           :: Maybe Bool
              , schemaExtensions           :: Extensions
              }
            deriving (Show, Eq, Generic)

data BoolOr a = IsBool Bool
              | IsNotBool a
              deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (BoolOr a) where
  parseJSON (Bool b) = pure $ IsBool b
  parseJSON v        = IsNotBool <$> parseJSON v

data Discriminator = Discriminator
                     { discriminatorPropertyName :: Text
                     , discriminatorMapping      :: Maybe (Map Text Text)
                     }
                   deriving (Show, Eq, Generic)

data XML = XML { xmlName       :: Maybe Text
               , xmlNamespace  :: Maybe Text
               , xmlPrefix     :: Maybe Text
               , xmlAttribute  :: Maybe Bool
               , xmlWrapped    :: Maybe Bool
               , xmlExtensions :: Extensions
               }
         deriving (Show, Eq, Generic)

-- Use `type` to determine which one of these to parse
data SecurityScheme = APIKeySS APIKeySecurityScheme
                    | HTTP_SS HTTPSecurityScheme
                    | OAuthSS OAuthSecurityScheme
                    | OpenIdConnectSS OpenIdConnectSecurityScheme
                    deriving (Show, Eq, Generic)

data APIKeySecurityScheme = APIKeySecurityScheme
                            { apiKeyDescription :: Maybe Text
                            , apiKeyName        :: Text
                            , apiKeyIn          :: APIKeySecuritySchemeIn
                            , apiKeyExtensions  :: Extensions
                            }
                          deriving (Show, Eq, Generic)

data APIKeySecuritySchemeIn = APIKeySecuritySchemeInQuery
                            | APIKeySecuritySchemeInHeader
                            | APIKeySecuritySchemeInCookie
                            deriving (Show, Eq, Generic)

data HTTPSecurityScheme = HTTPSecurityScheme
                          { httpScheme       :: Text
                          , httpBearerFormat :: Maybe Text
                          , httpExtensions   :: Extensions
                          }
                        deriving (Show, Eq, Generic)

data OAuthSecurityScheme = OAuthSecurityScheme
                           { oauthFlows :: OAuthFlows }
                         deriving (Show, Eq, Generic)

data OAuthFlows = OAuthFlows
                  { oauthFlowsImplicit          :: Maybe OAuthFlow
                  , oauthFlowsPassword          :: Maybe OAuthFlow
                  , oauthFlowsClientCredentials :: Maybe OAuthFlow
                  , oauthFlowsAuthorizationCode :: Maybe OAuthFlow
                  , oauthFlowsExtensions        :: Extensions
                  }
                deriving (Show, Eq, Generic)

data OAuthFlow = OAuthFlow
                 { oauthFlowAuthorizationUrl :: Text
                 , oauthFlowTokenUrl         :: Text
                 , oauthFlowRefershUrl       :: Maybe Text
                 , oauthFlowScopes           :: Map Text Text
                 , oauthFlowExtensions       :: Extensions
                 }
               deriving (Show, Eq, Generic)

data OpenIdConnectSecurityScheme = OpenIdConnectSecurityScheme
                                   { openIdConnectUrl :: Text }
                                 deriving (Show, Eq, Generic)

data SecurityRequirement = SecurityRequirements (Map Text [Text])
                         deriving (Show, Eq, Generic)
data Extensions = Extensions (HM.HashMap Text Value)
                deriving (Show, Eq, Generic)

instance FromJSON Extensions where
  parseJSON (Object o) = pure $ Extensions o
  parseJSON v          = typeMismatch "Object" v

instance FromJSON OpenAPISpec where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "spec"

instance FromJSON Info where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "info"

instance FromJSON Contact where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "contact"

instance FromJSON License where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "license"

instance FromJSON Server where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "server"

instance FromJSON ServerVariable where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "serverVariable"

instance FromJSON Components where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "component"

instance FromJSON Paths where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "paths"

instance FromJSON PathItem where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "path"

instance FromJSON Operation where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "operation"

instance FromJSON ExternalDocs where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "externalDocs"

instance FromJSON Parameter where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "parameter"

instance FromJSON RequestBody where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "requestBody"

instance FromJSON MediaType where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "mediaType"

instance FromJSON Encoding where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "encoding"

instance FromJSON Responses where
  parseJSON =
    withObject "Responses"
    $ \v -> do
        defaultResponses <- v .: "default" <|> pure Nothing
        byStatus <- parseJSON $ Object v
        let exts = extractExtensions v
        pure $ Responses defaultResponses byStatus (Extensions exts)

instance FromJSON Response where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "response"

instance FromJSON Callback where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "callback"

instance FromJSON Example where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "example"

instance FromJSON Link where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "link"

instance FromJSON Header where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "header"

instance FromJSON Tag where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "tag"

instance FromJSON Reference where
  parseJSON (Object o) = case HM.lookup "$ref" o of
                           Just (String ref) -> pure $ Reference ref
                           _ -> fail "expected to find $ref in Object"
  parseJSON v = typeMismatch "Object" v

instance FromJSON Schema where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "schema"

instance FromJSON Discriminator where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON XML where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "xml"

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

instance FromJSON OAuthSecurityScheme where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "oauth"

instance FromJSON HTTPSecurityScheme where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "http"

instance FromJSON APIKeySecurityScheme where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "apiKey"

instance FromJSON OpenIdConnectSecurityScheme where
  parseJSON = parseExtensibleJSON defaultOptions

instance FromJSON OAuthFlows where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "oauthFlows"

instance FromJSON OAuthFlow where
  parseJSON = parseExtensibleJSON $ optsParseWithoutPrefix "oauthFlow"

instance FromJSON SecurityRequirement where
  parseJSON = fmap SecurityRequirements . parseJSON

instance FromJSON APIKeySecuritySchemeIn where
  parseJSON (String "cookie") = pure $ APIKeySecuritySchemeInCookie
  parseJSON (String "header") = pure $ APIKeySecuritySchemeInHeader
  parseJSON (String "query")  = pure $ APIKeySecuritySchemeInQuery
  parseJSON v = typeMismatch "expected cookie, header or query" v

instance FromJSON ParameterIn where
  parseJSON (String "cookie") = pure $ ParameterInCookie
  parseJSON (String "header") = pure $ ParameterInHeader
  parseJSON (String "path")   = pure $ ParameterInPath
  parseJSON (String "query")  = pure $ ParameterInQuery
  parseJSON v = typeMismatch "expected cookie, header, path or query" v

optsParseWithoutPrefix :: String -> Options
optsParseWithoutPrefix prefix = defaultOptions { fieldLabelModifier = removePrefix prefix }

removePrefix :: String -> String -> String
removePrefix prefix input = maybe input id $ go prefix input
  where
    go [] (i:is) = Just $ toLower i:is
    go _ [] = Nothing
    go (prefixChar:remainingPrefix) (inputChar:remainingInput) =
      if prefixChar == inputChar
      then go remainingPrefix remainingInput
      else Nothing

parseExtensibleJSON :: (Generic a, GFromJSON Zero (Rep a)) => Options -> Value -> Parser a
parseExtensibleJSON opts = do
  withObject "Extensible"
  $ \v ->
      let exts = extractExtensions v
      in  genericParseJSON opts  (Object $ HM.insert "extensions" (Object exts) v)

extractExtensions ::  HM.HashMap Text Value -> HM.HashMap Text Value
extractExtensions = HM.filterWithKey (\k _ -> "x-" `isPrefixOf` k)
