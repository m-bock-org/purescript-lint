module Lint.Internal.Upstream
  ( UpstreamPackage
  , upstreamPackages
  ) where

import Prelude

import Data.Array (filter, mapMaybe) as Array
import Data.Either (Either(..))
import Data.Either (hush) as Either
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String (split, stripSuffix) as Str
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff (attempt, error, throwError) as Aff
import Lint.Internal.Exposed (Exposed, decodeExposed, exposedFile)
import Lint.Internal.Spago (SpagoGitPackage)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic (expandGlobs)
import Node.Path (FilePath)
import Node.Path (concat, dirname) as Path

-- | A package this repository depends on, as far as a boundary is
-- | concerned: what it is called, where it came from, what it says it
-- | offers, and the modules it holds.
type UpstreamPackage =
  { name :: String
  , url :: String
  , exposes :: Maybe Exposed
  , modules :: Array String
  }

-- | Every dependency fetched from git, with what it says about itself
-- | if it says anything. Uses `packageAt`.
upstreamPackages :: Array SpagoGitPackage -> Aff (Array UpstreamPackage)
upstreamPackages fetched = do
  found <- expandGlobs "." [ Path.concat [ ".spago", "p", "**", exposedFile ] ]
  said <- traverse packageAt (Set.toUnfoldable found)
  pure (map (asUpstream said) fetched)

-- | Private. Used only by `upstreamPackages`. What one dependency
-- | said, if it is among the files that were found.
asUpstream
  :: Array { name :: String, exposes :: Exposed, modules :: Array String }
  -> SpagoGitPackage
  -> UpstreamPackage
asUpstream said fetched =
  case Array.filter (\one -> one.name == fetched.name) said of
    [ one ] ->
      { name: fetched.name, url: fetched.url, exposes: Just one.exposes, modules: one.modules }
    _ ->
      { name: fetched.name, url: fetched.url, exposes: Nothing, modules: [] }

-- | Private. Used only by `upstreamPackages`. Uses `nameOf`,
-- | `modulesUnder`. A file that will not parse stops the run: a
-- | boundary that quietly went unchecked is worse than one nobody
-- | claimed.
packageAt :: FilePath -> Aff { name :: String, exposes :: Exposed, modules :: Array String }
packageAt path = do
  said <- FS.readTextFile UTF8 path
  case decodeExposed path said of
    Left why -> Aff.throwError (Aff.error why)
    Right exposes -> do
      modules <- modulesUnder (Path.dirname path)
      pure { name: nameOf path, exposes, modules }

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
-- Only the dependencies fetched from git are listed at all. A package
-- from the registry is a stranger's, arrives by version rather than by
-- url, and is not something a repository of ours has an opinion about.
--
-- `url` is how a rule can tell one of ours from a stranger's, which
-- matters because the file cannot be mandatory for everybody. A package
-- of ours that has not said what it offers has forgotten to, and that
-- is a finding; a stranger's saying nothing is just a stranger.
--
-- A file that will not parse stops the run wherever it came from. The
-- alternative is a boundary that silently stopped being checked, which
-- is the failure this whole idea exists to prevent.
