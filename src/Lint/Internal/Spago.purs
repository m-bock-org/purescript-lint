module Lint.Internal.Spago
  ( spagoLsPackages
  , SpagoWorkspacePackage
  , SpagoPkg(..)
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Json.Decode
  ( DecodeJson
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

-- | A package this repo owns, as spago reports it.
type SpagoWorkspacePackage =
  { name :: String
  , path :: String
  }

-- | A package spago knows about. Only the workspace's own are ours to
-- | lint; registry and git dependencies fold together as `PkgOther`.
data SpagoPkg = PkgOther | PkgWorkspace SpagoWorkspacePackage

-- | Every package spago knows about, ours and its dependencies.
spagoLsPackages :: Aff (Array SpagoPkg)
spagoLsPackages = do
  raw <- runSpagoLsPackages
  case runDecodeFromString (decodeObjectWithKey decodeSpagoPkg) raw of
    Right obj -> pure (Obj.values obj)
    Left err -> Aff.throwError
      (Aff.error ("spago ls packages --json: " <> printJsonDecodeError err))

runSpagoLsPackages :: Aff String
runSpagoLsPackages = liftEffect do
  buf <- execSync "spago ls packages --json"
  Buffer.toString UTF8 buf

decodeSpagoPkg :: String -> DecodeJson SpagoPkg
decodeSpagoPkg name = ado
  tagged <- decodeAttempt (decodeRecord { type: decodeString })
  located <- decodeAttempt workspacePathOf
  in
    case tagged, located of
      Right { type: "workspace" }, Right { value: { path } } -> PkgWorkspace { name, path }
      _, _ -> PkgOther

workspacePathOf :: DecodeJson { value :: { path :: String } }
workspacePathOf = decodeRecord { value: decodeRecord { path: decodeString } }
