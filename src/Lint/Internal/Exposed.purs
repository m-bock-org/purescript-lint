module Lint.Internal.Exposed
  ( Exposed
  , exposedFile
  , decodeExposed
  , readExposed
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Json.Decode (DecodeJson, decodeArray, decodeString, runDecode)
import Data.Json.Decode.Record (decodeRecord)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff (attempt) as Aff
import Lint.Internal.Yaml as Yaml
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path

type Exposed = Array String

exposedFile :: String
exposedFile = "package.yaml"

-- | What one package offers, read from `package.yaml` beside its
-- | manifest. `Nothing`
-- | is a package that has not said, which is not the same as a package
-- | that offers nothing. Uses `decodeExposed`.
readExposed :: FilePath -> Aff (Either String (Maybe Exposed))
readExposed packagePath = do
  let path = Path.concat [ packagePath, exposedFile ]
  attempted <- Aff.attempt (FS.readTextFile UTF8 path)
  case attempted of
    Left _ -> pure (Right Nothing)
    Right text -> pure (map Just (decodeExposed path text))

-- | The file's contents, decoded. Uses `Yaml.parse`.
decodeExposed :: FilePath -> String -> Either String Exposed
decodeExposed path text = case Yaml.parse text of
  Left why -> Left (path <> ": " <> why)
  Right json -> case runDecode decodeFile json of
    Left why -> Left (path <> ": " <> show why)
    Right file -> Right file.exposes

-- | Private. Used only by `decodeExposed`.
decodeFile :: DecodeJson { exposes :: Exposed }
decodeFile = decodeRecord { exposes: decodeArray decodeString }

-- ## Context
--
-- What a package says about itself, beside the manifest that says what
-- it takes. Today one key, `exposes`; the shape is an object rather
-- than a bare list so the second key does not need a second file, and
-- unknown keys are ignored so a repository can carry one this build
-- does not know about yet.
--
-- Not in `spago.yaml`: a key spago does not know makes it skip the
-- whole file, and a skipped manifest takes its package out of the
-- workspace without an error anybody would connect to the cause.
--
-- One file per package rather than one for the repository, because a
-- file that sits in a package names no package - its location is the
-- name - and so nothing has to be kept in step with a rename.
--
-- `readExposed`
-- Absent reads as `Nothing` and not as `[]`. A package that has not
-- said anything is a package nobody has got to yet; a package that
-- says it exposes nothing has been thought about. Only a rule can
-- decide what to do about either, and it needs to be able to tell them
-- apart.
--
-- Present but unreadable is an error, for the same reason a mistyped
-- exemptions file is: a boundary that quietly stopped being checked is
-- worse than one that was never claimed.
