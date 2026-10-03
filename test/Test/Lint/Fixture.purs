-- | A workspace of two packages, for the specs that run the linter.
module Test.Lint.Fixture
  ( inFixture
  , fixtureFile
  , otherFile
  , packageFile
  ) where

import Prelude

import Data.Foldable (for_) as Foldable
import Effect.Aff (Aff)
import Effect.Aff (bracket) as Aff
import Effect.Class (liftEffect)
import Node.FS.Aff (copyFile, mkdir', mkdtemp, rm') as FS
import Node.FS.Perms (permsAll) as Perms
import Node.FS.Sync (exists) as Sync
import Node.OS (tmpdir) as OS
import Node.Path (concat) as Path
import Node.Process (chdir, cwd) as Process

-- | Where a spec runs from, and where it was before.
type Visit = { here :: String, there :: String }

-- | One fixture file at rest, and its name once materialised.
type Copied = { from :: String, to :: String }

-- | The module `pkg` holds, as a path from the fixture's root.
fixtureFile :: String
fixtureFile = "pkg/src/Fixture.purs"

-- | The module the second package holds: what a package-scoped exemption must not reach.
otherFile :: String
otherFile = "other/src/Other.purs"

-- | Where a package's own exemptions go, for the package named.
packageFile :: String -> String
packageFile package = Path.concat [ package, "lint-exemptions.yaml" ]

-- | Run a spec inside a fresh copy of the fixture, and come back.
-- | Uses `enterFixture`, `leaveFixture`.
inFixture :: Aff Unit -> Aff Unit
inFixture run = Aff.bracket enterFixture leaveFixture (const run)

-- | Private. Used only by `inFixture`. Uses `fixtureRoot`, `fixtureFiles`, `packageDirs`.
enterFixture :: Aff Visit
enterFixture = do
  (here :: String) <- liftEffect Process.cwd
  (root :: String) <- fixtureRoot
  (temp :: String) <- liftEffect OS.tmpdir
  (there :: String) <- FS.mkdtemp (Path.concat [ temp, "lint-fixture-" ])
  Foldable.for_ packageDirs \(package :: String) ->
    FS.mkdir' (Path.concat [ there, package, "src" ]) { recursive: true, mode: Perms.permsAll }
  Foldable.for_ fixtureFiles \({ from, to } :: Copied) ->
    FS.copyFile (Path.concat [ root, from ]) (Path.concat [ there, to ])
  liftEffect (Process.chdir there)
  pure { here, there }

-- | Private. Used only by `inFixture`.
leaveFixture :: Visit -> Aff Unit
leaveFixture { here, there } = do
  liftEffect (Process.chdir here)
  FS.rm' there
    { force: true
    , maxRetries: 0
    , recursive: true
    , retryDelay: 0
    }

-- | Private. Used only by `enterFixture`.
fixtureRoot :: Aff String
fixtureRoot = do
  (inRoot :: Boolean) <- liftEffect (Sync.exists "fixture")
  pure (if inRoot then "fixture" else Path.concat [ "purs", "lint", "fixture" ])

-- | Private. Used only by `enterFixture`.
packageDirs :: Array String
packageDirs = [ "pkg", "other" ]

-- | Private. Used only by `enterFixture`.
fixtureFiles :: Array Copied
fixtureFiles =
  [ { from: "spago.yaml.txt", to: "spago.yaml" }
  , { from: "spago.lock.txt", to: "spago.lock" }
  , { from: "pkg/spago.yaml.txt", to: "pkg/spago.yaml" }
  , { from: fixtureFile, to: fixtureFile }
  , { from: "other/spago.yaml.txt", to: "other/spago.yaml" }
  , { from: otherFile, to: otherFile }
  ]

-- ## Context
--
-- The specs that run the linter over a workspace run it here, and
-- never over the workspace this package belongs to. The fix loop
-- writes proposals into real files and puts them back, and a run that
-- dies in between - a timeout, a crash - leaves them as written; over
-- this repository's own sources that emptied two modules once. An
-- exemption spec writes the exemption file where it runs, which used
-- to be this package's own.
--
-- The fixture's files are stored with a `.txt` suffix where spago
-- would otherwise read them as a second workspace, and renamed on the
-- way into a copy under the system's temporary directory, which is
-- removed afterwards, success or not.
--
-- `fixtureRoot`: spago runs a suite from the workspace root, which is
-- this package's directory when it is a workspace of its own and
-- purescript-libs's root, two directories up, when it is one package
-- of many.
