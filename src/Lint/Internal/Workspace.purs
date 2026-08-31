module Lint.Internal.Workspace
  ( LocalPackage
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
import Lint.Internal.Rule (ModuleKind(..))
import Lint.Internal.Spago (SpagoPkg(..), spagoLsPackages)

-- | Every package this repo owns, as a structural map. Cheap to hold
-- | whole; a module's CST is read separately when something needs it.
type Workspace = { packages :: Array LocalPackage }

-- | One package: its spago name, its directory, and its modules.
type LocalPackage =
  { name :: String
  , path :: FilePath
  , modules :: Array WorkspaceModule
  }

-- | Where one module is, and which tree it came from.
type WorkspaceModule =
  { path :: FilePath
  , kind :: ModuleKind
  }

modulesOf :: FilePath -> Aff (Array WorkspaceModule)
modulesOf packagePath = do
  sourceModules <- modulesOfKind SourceModule (moduleGlob packagePath "src")
  testModules <- modulesOfKind TestModule (moduleGlob packagePath "test")
  pure (sourceModules <> testModules)

-- | The glob for one of a package's two module trees. Spago reports a
-- | root-level package's path as `./`, so the join has to normalise.
moduleGlob :: FilePath -> String -> String
moduleGlob packagePath tree = Path.concat [ packagePath, tree, "**", "*.purs" ]

modulesOfKind :: ModuleKind -> String -> Aff (Array WorkspaceModule)
modulesOfKind kind pattern = do
  paths <- expandGlobs "." [ pattern ]
  pure (map (\path -> { path, kind }) (Set.toUnfoldable paths))

-- | Ask spago which packages this repo owns, and find their modules.
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

toLocalPackage :: { name :: String, path :: FilePath } -> Aff LocalPackage
toLocalPackage { name, path } = do
  modules <- modulesOf path
  pure { name, path, modules }

-- | Parse one module, failing loudly if it does not parse.
readModule :: WorkspaceModule -> Aff (CST.Module Void)
readModule { path } = do
  src <- FS.readTextFile UTF8 path
  case CST.parseModule src of
    CST.ParseSucceeded m -> pure m
    CST.ParseSucceededWithErrors _ errs -> parseFailure path (NEA.head errs)
    CST.ParseFailed err -> parseFailure path err

parseFailure :: ∀ a. FilePath -> PositionedError -> Aff a
parseFailure path err = Aff.throwError (Aff.error (printPositionedError path err))

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

-- | Write a module back over its own file, after a rule rewrote it.
writeModule :: FilePath -> CST.Module Void -> Aff Unit
writeModule path m = FS.writeTextFile UTF8 path (CST.printModule m)
