-- | Rules that look at many modules at once rather than at one piece of
-- | syntax.
-- |
-- | A survey rule is handed a cheap structural map - module names,
-- | paths and kinds - rather than every module's CST, so a rule about
-- | how a package is laid out does not pay for parsing it. `perPackage`
-- | sees one package; `perWorkspace` sees them all.
-- |
-- | A survey rule reports findings.
module PureScript.Lint.Rule.Survey (module Exports) where

import PureScript.Lint.Internal.Survey
  ( class HasPageExclude
  , PackageLint
  , PackageRule
  , PackageSurvey
  , PageExemption
  , SurveyFinding
  , SurveyModule
  , WorkspaceLint
  , WorkspaceRule
  , WorkspaceSurvey
  , excludePages
  , perPackage
  , perWorkspace
  ) as Exports
