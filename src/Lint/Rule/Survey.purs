-- | Rules that look at many modules at once rather than at one piece of
-- | syntax.
-- |
-- | A survey rule is handed a cheap structural map - module names,
-- | paths and kinds - rather than every module's CST, so a rule about
-- | how a package is laid out does not pay for parsing it. `perPackage`
-- | sees one package; `perWorkspace` sees them all.
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
