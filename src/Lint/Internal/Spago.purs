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
  , JsonDecodeError
  , decodeArray
  , decodeAttempt
  , decodeObjectWithKey
  , decodeString
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
-- | what its own `spago.yaml` lists, not the transitive closure;
-- | `testDependencies` is what its `test` section lists, the same way.
type SpagoWorkspacePackage =
  { name :: String
  , path :: String
  , dependencies :: Array String
  , testDependencies :: Array String
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
  declaredForTests <- decodeAttempt workspaceTestDependenciesOf
  in
    case tagged, located of
      Right { type: "workspace" }, Right { value: { path } } ->
        PkgWorkspace
          { name
          , path
          , dependencies: dependenciesOr declared
          , testDependencies: testDependenciesOr declaredForTests
          }
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

-- | The test section's names, read the same way; a package without
-- | tests has no `test` section, and that is no dependencies.
-- | Private.
workspaceTestDependenciesOf
  :: DecodeJson { value :: { package :: { test :: { dependencies :: Array String } } } }
workspaceTestDependenciesOf = decodeRecord
  { value: decodeRecord
      { package: decodeRecord { test: decodeRecord { dependencies: decodeNames } } }
  }

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

-- | Private, depth 2. Used only by `decodeSpagoPkg`.
testDependenciesOr
  :: Either JsonDecodeError { value :: { package :: { test :: { dependencies :: Array String } } } }
  -> Array String
testDependenciesOr =
  Either.either (const []) (_.value >>> _.package >>> _.test >>> _.dependencies)
