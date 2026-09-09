module Lint.Internal.Foreign
  ( ForeignPackage
  , foreignPackages
  ) where

import Prelude

import Data.Array (mapMaybe) as Array
import Data.Either (Either(..))
import Data.Either (hush) as Either
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String (split, stripSuffix) as Str
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff (attempt) as Aff
import Lint.Internal.Exposed (Exposed, decodeExposed, exposedFile)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic (expandGlobs)
import Node.Path (FilePath)
import Node.Path (concat, dirname) as Path

-- | A package this repo depends on that says what it offers: where its
-- | checkout is, what it exposes, and the modules it holds.
type ForeignPackage =
  { name :: String
  , exposes :: Exposed
  , modules :: Array String
  }

-- | Every dependency carrying a `package.yaml`, which is every
-- | dependency of ours. Uses `packageAt`.
foreignPackages :: Aff (Array ForeignPackage)
foreignPackages = do
  found <- expandGlobs "." [ Path.concat [ ".spago", "p", "**", exposedFile ] ]
  read <- traverse packageAt (Set.toUnfoldable found)
  pure (Array.mapMaybe identity read)

-- | Private. Used only by `foreignPackages`. Uses `nameOf`,
-- | `modulesUnder`.
packageAt :: FilePath -> Aff (Maybe ForeignPackage)
packageAt path = do
  text <- Aff.attempt (FS.readTextFile UTF8 path)
  case Either.hush text of
    Nothing -> pure Nothing
    Just said -> case decodeExposed path said of
      Left _ -> pure Nothing
      Right exposes -> do
        modules <- modulesUnder (Path.dirname path)
        pure (Just { name: nameOf path, exposes, modules })

-- | Private, depth 2. Used only by `packageAt`. The package's name as
-- | spago cached it: `.spago/p/<name>/<ref>/...`.
nameOf :: FilePath -> String
nameOf path = case Str.split (Pattern "/") path of
  [ _, _, name ] -> name
  [ _, _, name, _ ] -> name
  segments -> case segments of
    [ _, _, name, _, _ ] -> name
    _ -> "a dependency"

-- | Private, depth 2. Used only by `packageAt`.
modulesUnder :: FilePath -> Aff (Array String)
modulesUnder root = do
  paths <- expandGlobs "." [ Path.concat [ root, "src", "**", "*.purs" ] ]
  named <- traverse moduleNameIn (Set.toUnfoldable paths)
  pure (Array.mapMaybe identity named)

-- | Private, depth 3. Used only by `modulesUnder`. Uses `declared`.
moduleNameIn :: FilePath -> Aff (Maybe String)
moduleNameIn path = do
  text <- Aff.attempt (FS.readTextFile UTF8 path)
  pure (map declared (Either.hush text))

-- | Private, depth 4. Used only by `moduleNameIn`. The name on the
-- | `module` line, without parsing the rest of the file.
declared :: String -> String
declared text =
  let
    named = Array.mapMaybe afterModule (Str.split (Pattern "\n") text)
  in
    case named of
      [ name ] -> name
      _ -> ""

-- | Private, depth 5. Used only by `declared`.
afterModule :: String -> Maybe String
afterModule line =
  let
    words = Str.split (Pattern " ") line
  in
    case words of
      [ "module", name ] -> Just (trimmed name)
      [ "module", name, _ ] -> Just (trimmed name)
      _ -> Nothing

-- | Private, depth 6. Used only by `afterModule`.
trimmed :: String -> String
trimmed name = case Str.stripSuffix (Pattern "\r") name of
  Just cut -> cut
  Nothing -> name

-- ## Context
--
-- A dependency that says what it offers, so the boundary holds across
-- repositories and not only inside one.
--
-- Found by looking for the file rather than by asking spago where a
-- package is: `spago ls packages --json` gives a git dependency its
-- name and ref and no path, and the path it caches to is a detail of
-- spago's own layout. A glob for `package.yaml` under `.spago/p` finds
-- every checkout that has one, at whatever depth its subdirectory sits.
--
-- A dependency without the file is not policed, which is every package
-- from the registry and every one of ours that has not adopted it yet.
-- An unreadable one is skipped rather than fatal - it is somebody
-- else's file, and a repository should not be stopped by it. Ours are
-- read strictly, because those we own.
