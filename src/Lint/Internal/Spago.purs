module Lint.Internal.Spago
  ( spagoLsPackages
  , SpagoWorkspacePackage
  , SpagoPkg(..)
  ) where

import Prelude

import Data.Array (mapMaybe) as Array
import Data.Either (Either(..))
import Data.Either (either, hush) as Either
import Data.Json.Decode
  ( DecodeJson
  , decodeArray
  , decodeAttempt
  , decodeObjectWithKey
  , decodeString
  , JsonDecodeError
  , printJsonDecodeError
  , runDecodeFromString
  )
import Data.Json.Decode.Record (decodeRecord)
import Effect.Aff (Aff)
import Effect.Aff (error, throwError) as Aff
import Effect.Class (liftEffect)
import Foreign.Object as Obj
import Node.Buffer as Buffer
import Node.ChildProcess (execSync)
import Node.Encoding (Encoding(..))

-- | A package this repo owns, as spago reports it. `dependencies` is
-- | what its own `spago.yaml` lists, not the transitive closure.
type SpagoWorkspacePackage =
  { name :: String
  , path :: String
  , dependencies :: Array String
  }

-- | A package spago knows about. Only the workspace's own are ours to
-- | lint; registry and git dependencies fold together as `PkgOther`.
data SpagoPkg = PkgOther | PkgWorkspace SpagoWorkspacePackage

-- | Every package spago knows about, ours and its dependencies.
-- | Uses `decodeSpagoPkg`.
spagoLsPackages :: Aff (Array SpagoPkg)
spagoLsPackages = do
  raw <- runSpagoLsPackages
  case runDecodeFromString (decodeObjectWithKey decodeSpagoPkg) raw of
    Right obj -> pure (Obj.values obj)
    Left err -> Aff.throwError
      (Aff.error ("spago ls packages --json: " <> printJsonDecodeError err))

-- | Private.
runSpagoLsPackages :: Aff String
runSpagoLsPackages = liftEffect do
  buf <- execSync "spago ls packages --json"
  Buffer.toString UTF8 buf

-- | Private. Used only by `spagoLsPackages`. Uses `dependenciesOr`.
decodeSpagoPkg :: String -> DecodeJson SpagoPkg
decodeSpagoPkg name = ado
  tagged <- decodeAttempt (decodeRecord { type: decodeString })
  located <- decodeAttempt workspacePathOf
  declared <- decodeAttempt workspaceDependenciesOf
  in
    case tagged, located of
      Right { type: "workspace" }, Right { value: { path } } ->
        PkgWorkspace { name, path, dependencies: dependenciesOr declared }
      _, _ -> PkgOther

-- | Private.
workspacePathOf :: DecodeJson { value :: { path :: String } }
workspacePathOf = decodeRecord { value: decodeRecord { path: decodeString } }

-- | Only the names. A dependency spago.yaml can spell as a bare string
-- | or as a map with a version range, and the range is nothing this
-- | reads, so an entry that is not a plain string is dropped rather
-- | than failing the whole package.
-- | Private.
workspaceDependenciesOf :: DecodeJson { value :: { package :: { dependencies :: Array String } } }
workspaceDependenciesOf = decodeRecord
  { value: decodeRecord { package: decodeRecord { dependencies: decodeNames } } }

-- | Private. Uses `keepNames`.
decodeNames :: DecodeJson (Array String)
decodeNames = map keepNames (decodeArray (decodeAttempt decodeString))

-- | Private. Used only by `decodeNames`.
keepNames :: Array (Either JsonDecodeError String) -> Array String
keepNames = Array.mapMaybe Either.hush

-- | Private, depth 2. Used only by `decodeSpagoPkg`.
dependenciesOr
  :: Either JsonDecodeError { value :: { package :: { dependencies :: Array String } } }
  -> Array String
dependenciesOr = Either.either (const []) (_.value >>> _.package >>> _.dependencies)
