module PureScript.Lint.Workspace
  ( LocalPackage
  , ModuleKind(..)
  , Workspace
  , WorkspaceModule
  , getWorkspace
  , moduleGlob
  , readModule
  , writeModule
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff (error, throwError) as Aff
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic (expandGlobs)
import Node.Path (FilePath)
import Node.Path (concat) as Path
import PureScript.CST (RecoveredParserResult(..), parseModule, printModule) as CST
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Parser.Monad (PositionedError)
import PureScript.CST.Types (Module) as CST
import PureScript.Lint.Spago (SpagoPkg(..), spagoLsPackages)

type Workspace = { packages :: Array LocalPackage }

type LocalPackage =
  { name :: String
  , path :: FilePath
  , modules :: Array WorkspaceModule
  }

type WorkspaceModule =
  { path :: FilePath
  , kind :: ModuleKind
  }

data ModuleKind = SourceModule | TestModule

derive instance eqModuleKind :: Eq ModuleKind

instance showModuleKind :: Show ModuleKind where
  show = case _ of
    SourceModule -> "SourceModule"
    TestModule -> "TestModule"

-- | Private, depth 2. Used only by `toLocalPackage`. Uses `modulesOfKind`.
modulesOf :: FilePath -> Aff (Array WorkspaceModule)
modulesOf packagePath = do
  sourceModules <- modulesOfKind SourceModule (moduleGlob packagePath "src")
  testModules <- modulesOfKind TestModule (moduleGlob packagePath "test")
  pure (sourceModules <> testModules)

-- | The glob for one of a package's two module trees. Built with
-- | `Node.Path.concat` rather than string append: spago reports a
-- | root-level package's path as `./`, and appending gives `.//src/...`,
-- | which matches nothing at all.
moduleGlob :: FilePath -> String -> String
moduleGlob packagePath tree = Path.concat [ packagePath, tree, "**", "*.purs" ]

-- | Private, depth 3. Used only by `modulesOf`.
modulesOfKind :: ModuleKind -> String -> Aff (Array WorkspaceModule)
modulesOfKind kind pattern = do
  paths <- expandGlobs "." [ pattern ]
  pure (map (\path -> { path, kind }) (Set.toUnfoldable paths))

-- | Uses `toLocalPackage`.
getWorkspace :: Aff Workspace
getWorkspace =
  let
    workspaceOnly = case _ of
      PkgWorkspace p -> Just p
      PkgOther -> Nothing

  in
    do
      pkgs <- spagoLsPackages
      packages <- traverse toLocalPackage (Array.mapMaybe workspaceOnly pkgs)
      pure { packages }

-- | Private. Used only by `getWorkspace`. Uses `modulesOf`.
toLocalPackage :: { name :: String, path :: FilePath } -> Aff LocalPackage
toLocalPackage { name, path } = do
  modules <- modulesOf path
  pure { name, path, modules }

-- | Uses `parseFailure`.
readModule :: WorkspaceModule -> Aff (CST.Module Void)
readModule { path } = do
  src <- FS.readTextFile UTF8 path
  case CST.parseModule src of
    CST.ParseSucceeded m -> pure m
    CST.ParseSucceededWithErrors _ errs -> parseFailure path (NEA.head errs)
    CST.ParseFailed err -> parseFailure path err

-- | Private. Used only by `readModule`. Uses `printPositionedError`.
parseFailure :: ∀ a. FilePath -> PositionedError -> Aff a
parseFailure path err = Aff.throwError (Aff.error (printPositionedError path err))

-- | Private, depth 2. Used only by `parseFailure`.
printPositionedError :: FilePath -> PositionedError -> String
printPositionedError path { position, error: err } =
  fold
    [ path
    , ":"
    , show position.line
    , ":"
    , show position.column
    , ": "
    , printParseError err
    ]

writeModule :: FilePath -> CST.Module Void -> Aff Unit
writeModule path m = FS.writeTextFile UTF8 path (CST.printModule m)

-- Context: the shape of a Spago workspace, as far as linting cares:
-- the local packages a workspace owns and where each of their modules
-- live. Structure only - a module's CST is read separately, by
-- whatever needs it, so a whole workspace stays cheap to hold. Workspace
-- holds every local package the workspace owns - not spago's registry
-- or git dependencies, only packages this repo can actually lint.
-- `LocalPackage.path` is the package's own directory as spago reports
-- it (e.g. `src/packages/parser`), kept because a package-level
-- rule may care where a package sits rather than only what it is
-- called - the two do not have to agree, and the directory is what a
-- reader actually browses.
-- LocalPackage's `name` is the package's own kebab-case `spago.yaml`
-- name (e.g. `"my-parser"`) - the same string a package-boundary rule
-- derives each package's namespace root from.
--
-- WorkspaceModule tracks just where the module is, not what is in it.
-- A `Workspace` is a cheap structural map of the repo that can be
-- held whole; the CST of any one module is read separately, when
-- something actually needs it. This carried a deferred CST until
-- 2026-08-26. The goal was right - never hold every module's CST in
-- memory at once - but expressing it as a thunk inside the type meant
-- the type could no longer say whether a module had been loaded, or
-- modified, or was still untouched on disk, which is exactly what a
-- writer needs to know. Keeping the workspace purely structural gets
-- the same memory property with none of that ambiguity. ModuleKind
-- tracks which of a package's two module trees a `WorkspaceModule`
-- came from - a rule needs this to, say, hold test modules to a
-- looser standard than source, or skip one tree entirely.
--
-- modulesOf reads `src` and `test` both, deliberately: a package's
-- own tests are part of the package for linting purposes (the
-- `.Internal` rule this is being built for explicitly allows a
-- package's tests to import its own internals, which it can only
-- check if it can see them). getWorkspace discovers every local
-- package in the workspace and every module within each, without
-- reading a single module's contents.
--
-- readModule fails on anything short of a clean parse - a
-- `CST.Module Void` is a promise that parsing produced no errors at
-- all, so a recovered-but-imperfect parse has nowhere to put its
-- errors except a thrown one here. writeModule is deliberately not a
-- `writeWorkspace :: Workspace -> Effect Unit`, which looks like the
-- natural counterpart to `getWorkspace` but cannot work: nothing in a
-- `Workspace` distinguishes a module a rule rewrote from one that was
-- never even loaded, so a bulk write would rewrite every file.
-- Parse-then-print is not guaranteed byte-identical (comment and
-- formatting placement), so untouched modules could come back
-- reformatted - churned mtimes, broken incremental builds, and a diff
-- full of changes nobody asked for. Per-module writing needs none of
-- that bookkeeping: whoever decided to change a module is the one
-- calling this, so "which modules are dirty" never has to be
-- represented at all.
