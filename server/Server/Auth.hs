module Server.Auth
  ( validateJWT
  , JWKStore
  , newJWKStore
  ) where

import Control.Concurrent.MVar
import Control.Lens ((^.))
import Control.Monad.Except (ExceptT, runExceptT)
import Crypto.JOSE.JWK (JWK, JWKSet(..))
import Crypto.JWT
  ( JWTError, JWTValidationSettings, SignedJWT, ClaimsSet
  , defaultJWTValidationSettings, verifyClaims
  , claimSub, decodeCompact
  )
import Data.Aeson (eitherDecode, toJSON, Value(..))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (newManager, httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Cached JWK Set fetched from the Supabase JWKS endpoint.
data JWKStore = JWKStore
  { jwksUrl   :: !String
  , jwksCache :: MVar (Maybe [JWK])
  }

-- | Create a new JWK store that will fetch keys from the given Supabase URL.
newJWKStore :: Text -> IO JWKStore
newJWKStore supabaseUrl = do
  let url = T.unpack supabaseUrl <> "/auth/v1/.well-known/jwks.json"
  cache <- newMVar Nothing
  pure (JWKStore url cache)

-- | Fetch (or return cached) JWK keys from the JWKS endpoint.
fetchKeys :: JWKStore -> IO (Either Text [JWK])
fetchKeys store = do
  cached <- readMVar (jwksCache store)
  case cached of
    Just keys -> pure (Right keys)
    Nothing -> do
      manager <- newManager tlsManagerSettings
      req <- parseRequest (jwksUrl store)
      resp <- httpLbs req manager
      case eitherDecode (responseBody resp) of
        Left err -> pure (Left (T.pack err))
        Right (JWKSet keys) -> do
          _ <- swapMVar (jwksCache store) (Just keys)
          pure (Right keys)

-- | Validate a Supabase JWT using JWKS and extract the user UUID from the @sub@ claim.
validateJWT :: JWKStore -> Text -> IO (Either Text Text)
validateJWT store token = do
  keysResult <- fetchKeys store
  case keysResult of
    Left err -> pure (Left ("Failed to fetch JWKS: " <> err))
    Right keys -> do
      let settings = defaultJWTValidationSettings (const True)
          compact = LBS.fromStrict (TE.encodeUtf8 token)
      tryKeys settings compact keys

-- | Try verifying the JWT against each key in the set until one succeeds.
tryKeys :: JWTValidationSettings -> LBS.ByteString -> [JWK] -> IO (Either Text Text)
tryKeys _ _ [] = pure (Left "No matching key found in JWKS")
tryKeys settings compact (k:ks) = do
  result <- runExceptT (verify k settings compact)
  case result of
    Left _ -> tryKeys settings compact ks
    Right claims ->
      case claims ^. claimSub of
        Nothing  -> pure (Left "JWT missing sub claim")
        Just sub -> case toJSON sub of
          String txt -> pure (Right txt)
          _          -> pure (Left "JWT sub is not a string")

-- | Helper with explicit type signature to resolve jose type ambiguity.
verify :: JWK -> JWTValidationSettings -> LBS.ByteString -> ExceptT JWTError IO ClaimsSet
verify jwk settings compact = do
  jwt <- decodeCompact compact
  verifyClaims settings jwk (jwt :: SignedJWT)
