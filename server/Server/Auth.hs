module Server.Auth
  ( validateJWT
  ) where

import Control.Lens ((^.))
import Control.Monad.Except (ExceptT, runExceptT)
import Crypto.JOSE.JWK (JWK, fromOctets)
import Crypto.JWT
  ( JWTError, JWTValidationSettings, SignedJWT, ClaimsSet
  , defaultJWTValidationSettings, verifyClaims
  , claimSub, decodeCompact
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Validate a Supabase JWT and extract the user UUID from the @sub@ claim.
-- The secret is the raw SUPABASE_JWT_SECRET (used for HS256).
validateJWT :: ByteString -> Text -> IO (Either Text Text)
validateJWT secret token = do
  let jwk = fromOctets secret
      settings = defaultJWTValidationSettings (const True)
      compact = LBS.fromStrict (TE.encodeUtf8 token)
  result <- runExceptT (verify jwk settings compact)
  case result of
    Left err -> pure (Left (T.pack (show err)))
    Right claims ->
      case claims ^. claimSub of
        Nothing  -> pure (Left "JWT missing sub claim")
        Just sub -> pure (Right (T.pack (show sub)))

-- | Helper with explicit type signature to resolve jose type ambiguity.
verify :: JWK -> JWTValidationSettings -> LBS.ByteString -> ExceptT JWTError IO ClaimsSet
verify jwk settings compact = do
  jwt <- decodeCompact compact
  verifyClaims settings jwk (jwt :: SignedJWT)
