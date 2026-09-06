-- | Rules that look at many modules at once rather than at one piece of
-- | syntax.
-- |
-- | A survey rule is handed a cheap structural map - module names,
-- | paths and kinds, each module's header, plus what each package's
-- | own `spago.yaml` depends on - rather than every module's body, so
-- | a rule about how a package is laid out does not hold every
-- | declaration in it. The header is what crosses a module's
-- | boundary: its export list and its imports, name by name.
-- | `perPackage` sees one package; `perWorkspace` sees them all, which
-- | is what a rule about the dependency graph between packages needs.
-- |
-- | A survey rule reports findings.
module Lint.Rule.Survey (module Exports) where

import Lint.Internal.Survey
  ( class HasSubjectIgnore
  , PackageLint
  , PackageRule
  , PackageSurvey
  , Subject(..)
  , SubjectExemption
  , SurveyFinding
  , SurveyModule
  , WorkspaceLint
  , WorkspaceRule
  , WorkspaceSurvey
  , ignoreSubjects
  , perPackage
  , perPackage_
  , perWorkspace
  , perWorkspace_
  ) as Exports
