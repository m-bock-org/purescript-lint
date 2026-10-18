module Lint.Internal.Interface
  ( Interface
  , InterfaceModule
  , ManifestPackage
  , printInterface
  , readInterface
  , readManifest
  ) where

import Overture

import Data.Json.Codec (JsonCodec, codecArray, codecRefine, codecString, decoder, encoder)
import Data.Json.Codec.Record (codecRecord)
import Data.Json.Decode (JsonDecodeError(..), printJsonDecodeError, runDecodeFromString)
import Data.Json.Encode (runEncodeToString)
import Effect.Aff (Aff)
import Effect.Aff (error, throwError) as Aff
import Lint.Internal.Rule (ModuleKind(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile) as FS
import Node.Path (FilePath)

-- | One package as the manifest says it, in place of what `spago ls packages` says.
type ManifestPackage = { name :: String, path :: FilePath, dependencies :: Array String }

-- | One module as another package sees it: its name, where it is, and its header text.
type InterfaceModule = { moduleName :: String, path :: FilePath, kind :: ModuleKind, header :: String }

-- | What one package offers the rest: every module's header, and nothing of its bodies.
type Interface = { package :: String, modules :: Array InterfaceModule }

-- | The manifest at this path; a broken one stops the run.
readManifest :: FilePath -> Aff (Array ManifestPackage)
readManifest path = do
  (said :: String) <- FS.readTextFile UTF8 path
  case runDecodeFromString (decoder codecManifest) said of
    Left why -> Aff.throwError (Aff.error (path <> ": " <> printJsonDecodeError why))
    Right manifest -> pure manifest.packages

-- | The interface at this path; a broken one stops the run.
readInterface :: FilePath -> Aff Interface
readInterface path = do
  (said :: String) <- FS.readTextFile UTF8 path
  case runDecodeFromString (decoder codecInterface) said of
    Left why -> Aff.throwError (Aff.error (path <> ": " <> printJsonDecodeError why))
    Right interface -> pure interface

-- | An interface as the file `readInterface` reads.
printInterface :: Interface -> String
printInterface = runEncodeToString (encoder codecInterface)

codecManifest :: JsonCodec { packages :: Array ManifestPackage }
codecManifest = codecRecord
  { packages:
      codecArray codecManifestPackage
  }

codecManifestPackage :: JsonCodec ManifestPackage
codecManifestPackage = codecRecord
  { name: codecString
  , path: codecString
  , dependencies: codecArray codecString
  }

codecInterface :: JsonCodec Interface
codecInterface = codecRecord
  { package: codecString
  , modules: codecArray codecInterfaceModule
  }

codecInterfaceModule :: JsonCodec InterfaceModule
codecInterfaceModule = codecRecord
  { moduleName: codecString
  , path: codecString
  , kind: codecRefine readKind writeKind codecString
  , header: codecString
  }

readKind :: String -> Either JsonDecodeError ModuleKind
readKind = case _ of
  "src" -> Right SourceModule
  "test" -> Right TestModule
  _ -> Left (TypeMismatch "a module kind is `src` or `test`")

writeKind :: ModuleKind -> String
writeKind = case _ of
  SourceModule -> "src"
  TestModule -> "test"

-- ## Context
--
-- The lint check used to be one derivation over every module of the workspace, so a line
-- anywhere re-read all of them. It is one check per package now, and a package's rules
-- that look past it - which modules a neighbour exposes, which are product code - read
-- the neighbour's interface: what crosses its boundary, written once per package and
-- rebuilt only when that package's sources change. The header is kept as text and parsed
-- again where it is read, so the reader sees the same `ModuleHeader` a full parse gives.
