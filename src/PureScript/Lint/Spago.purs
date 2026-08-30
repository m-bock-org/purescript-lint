module PureScript.Lint.Spago
  ( spagoLsPackages
  , SpagoWorkspacePackage
  , SpagoPkg(..)
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Json.Decode
  ( DecodeJson
  , decodeAttempt
  , decodeFromString
  , decodeObjectWithKey
  , decodeString
  , printJsonDecodeError
  )
import Data.Json.Decode.Record (decodeRecord)
import Effect.Aff (Aff)
import Effect.Aff (error, throwError) as Aff
import Effect.Class (liftEffect)
import Foreign.Object as Obj
import Node.Buffer as Buffer
import Node.ChildProcess (execSync)
import Node.Encoding (Encoding(..))

type SpagoWorkspacePackage =
  { name :: String
  , path :: String
  }

data SpagoPkg = PkgOther | PkgWorkspace SpagoWorkspacePackage

-- | Uses `decodeSpagoPkg`.
spagoLsPackages :: Aff (Array SpagoPkg)
spagoLsPackages = do
  raw <- runSpagoLsPackages
  case decodeFromString (decodeObjectWithKey decodeSpagoPkg) raw of
    Right obj -> pure (Obj.values obj)
    Left err -> Aff.throwError
      (Aff.error ("spago ls packages --json: " <> printJsonDecodeError err))

-- | Private.
runSpagoLsPackages :: Aff String
runSpagoLsPackages = liftEffect do
  buf <- execSync "spago ls packages --json"
  Buffer.toString UTF8 buf

-- | Private. Used only by `spagoLsPackages`.
decodeSpagoPkg :: String -> DecodeJson SpagoPkg
decodeSpagoPkg name = ado
  tagged <- decodeAttempt (decodeRecord { type: decodeString })
  located <- decodeAttempt workspacePathOf
  in
    case tagged, located of
      Right { type: "workspace" }, Right { value: { path } } -> PkgWorkspace { name, path }
      _, _ -> PkgOther

-- | Private.
workspacePathOf :: DecodeJson { value :: { path :: String } }
workspacePathOf = decodeRecord { value: decodeRecord { path: decodeString } }

-- Context: the one place that shells out to `spago`. Asking spago
-- rather than reading `spago.yaml` files ourselves: it already knows
-- which packages are the workspace's own, and resolving that by hand
-- would mean reimplementing its config discovery. SpagoPkg's `--json`
-- tags each entry `registry`, `git` or `workspace` - only the last is
-- ours to lint, and only it carries a `path` at all. `PkgOther` folds
-- `registry`/`git` together rather than keeping them distinct:
-- nothing here treats a dependency differently based on where it came
-- from, only on whether it belongs to us. spagoLsPackages runs
-- `spago ls packages --json` and decodes it into one `SpagoPkg` per
-- package. `execSync`, not `Aff`-native process spawning - this runs
-- once per lint invocation, not on a hot path, so the simplicity of a
-- blocking call outweighs any latency it costs.
