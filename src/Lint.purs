module Lint (LintOptions, runLinter, runLinterWith) where

import Prelude

import Control.Monad.State (State, modify_, runState)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Foldable (fold, for_, sum)
import Data.String.Common (joinWith, split) as String
import Data.String.Pattern (Pattern(..))
import Data.Maybe (Maybe(..))
import Data.Maybe (isNothing) as Maybe
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import PureScript.CST.Traversal (defaultVisitorM, rewriteDeclBottomUpM)
import PureScript.CST.Types
  ( Declaration(..)
  , Expr
  , Ident(..)
  , Labeled(..)
  , Module(..)
  , ModuleBody(..)
  , ModuleHeader(..)
  , ModuleName(..)
  , Name(..)
  ) as CST
import Lint.Internal.Rule
  ( ExprRule
  , Grouped
  , Finding
  , LintContext
  , ModuleExemption
  , RuleOutcome
  , runRules
  )
import Lint.Internal.RuleSet (FlatRules, Rule, flattenRules)
import Lint.Internal.Survey (PackageSurvey, SurveyModule, runSurveyRules)
import Lint.Internal.Workspace (WorkspaceModule)
import Lint.Internal.Workspace as Workspace

-- | Run a rule set over the Spago workspace in the current directory,
-- | reporting everything it finds. `true` when the workspace is clean.
-- |
-- | A rule that rewrites is applied: the module is written back.
runLinter :: Array Rule -> Aff Boolean
runLinter = runLinterWith { skipModules: [] }

-- | `runLinter` with options.
runLinterWith :: LintOptions -> Array Rule -> Aff Boolean
runLinterWith { skipModules } rules = do
  workspace <- Workspace.getWorkspace
  let
    flatRules = flattenRules rules
    configured = { skipModules, flatRules }
    moduleCount = Array.length (Array.concatMap _.modules workspace.packages)
  scanned <- for workspace.packages \pkg -> do
    perModule <- for pkg.modules (lintModule configured pkg.name)
    let
      survey =
        { packageName: pkg.name
        , packagePath: pkg.path
        , dependencies: pkg.dependencies
        , modules: map _.surveyed perModule
        }
    pure
      { survey
      , violations: sum (map _.violations perModule)
      , located: Array.concatMap _.located perModule
      }
  surveyed <- reportSurvey configured (map _.survey scanned)
  let total = sum (map _.violations scanned) + surveyed
  printByRule (Array.concatMap _.located scanned)
  printSummary total
    (Array.length (Array.nub (map _.moduleName (Array.concatMap _.located scanned))))
    moduleCount
  pure (total == 0)

reportSurvey :: Configured -> Array PackageSurvey -> Aff Int
reportSurvey { flatRules } surveys = do
  let
    forPackage s = map (append (s.packageName <> ": "))
      (runSurveyRules flatRules.packages s)
    perPackage = Array.concatMap forPackage surveys
    perWorkspace = runSurveyRules flatRules.workspaces { packages: surveys }
    findings = perPackage <> perWorkspace
  for_ findings \msg -> log ("  " <> msg)
  pure (Array.length findings)

-- | `skipModules` names modules no rule should see at all.
type LintOptions = { skipModules :: Array ModuleExemption }

type Configured =
  { skipModules :: Array ModuleExemption
  , flatRules :: FlatRules
  }

type ModuleScan =
  { surveyed :: SurveyModule
  , violations :: Int
  , located :: Array Located
  }

-- | One finding, and the module it was found in.
type Located = { moduleName :: String, finding :: Finding }

-- | Findings grouped by the rule that made them: what the rule wants,
-- | then every place it was not met.
printByRule :: Array Located -> Aff Unit
printByRule located =
  let
    groups = Array.groupBy (\a b -> a.finding.rule.name == b.finding.rule.name)
      (Array.sortWith (_.finding.rule.name) located)
  in
    for_ groups \group -> do
      let
        rule = (NEA.head group).finding.rule
        hints = Array.nub (Array.catMaybes (map _.finding.hint (NEA.toArray group)))
        sharedHint = if Array.length hints == 1 then Array.head hints else Nothing
      log ""
      log
        ( "● " <> String.joinWith " » "
            (Array.snoc (NEA.head group).finding.groups rule.name)
        )
      log ("    " <> rule.description)
      for_ sharedHint \h -> log ("    hint: " <> h)
      for_ rule.examples \examples -> do
        for_ examples.config (labelled "with")
        for_ examples.good (labelled "good")
        for_ examples.bad (labelled "bad ")
      for_ (NEA.toArray group) \{ moduleName, finding } -> do
        log ""
        log ("  ◦ " <> moduleName)
        log ("      " <> finding.message)
        when (Maybe.isNothing sharedHint) do
          for_ finding.hint \h -> log ("      hint: " <> h)

-- | One example under its label, with anything after the first line
-- | indented to sit under it.
labelled :: String -> String -> Aff Unit
labelled label text =
  for_ (Array.mapWithIndex indent (String.split (Pattern "\n") text)) log
  where
  indent 0 line = "      " <> label <> "  " <> line
  indent _ line = "            " <> line

-- | The one-line total, after everything else.
printSummary :: Int -> Int -> Int -> Aff Unit
printSummary total withFindings moduleCount = do
  log ""
  log $ fold
    [ "Summary: "
    , show total
    , if total == 1 then " finding in " else " findings in "
    , show withFindings
    , " of "
    , show moduleCount
    , " modules"
    ]

lintModule :: Configured -> String -> WorkspaceModule -> Aff ModuleScan
lintModule { skipModules, flatRules } packageName workspaceModule = do
  original <- Workspace.readModule workspaceModule
  let
    context :: LintContext
    context =
      { packageName
      , moduleName: moduleNameOf original
      , declarationName: Nothing
      , path: workspaceModule.path
      , kind: workspaceModule.kind
      }
    surveyed =
      { moduleName: context.moduleName, path: context.path, kind: context.kind }
  if Array.any (\g -> g.appliesTo context) skipModules then
    pure { surveyed, violations: 0, located: [] }
  else do
    let
      afterModules = runRules context flatRules.modules original
      afterDeclarations = rewriteDecls context afterModules.result
        (\ctx -> runRules ctx flatRules.declarations)
      afterExpressions = rewriteDecls context afterDeclarations.result
        (lintExprsInDecl flatRules.expressions)
      violations = afterModules.violations <> afterDeclarations.violations <>
        afterExpressions.violations
      fixed = afterModules.fixed || afterDeclarations.fixed || afterExpressions.fixed
    pure unit
    when fixed (Workspace.writeModule workspaceModule.path afterExpressions.result)
    pure
      { surveyed
      , violations: Array.length violations
      , located: map (\f -> { moduleName: context.moduleName, finding: f }) violations
      }

type PerDeclaration =
  LintContext -> CST.Declaration Void -> RuleOutcome (CST.Declaration Void)

declarationNameOf :: CST.Declaration Void -> Maybe String
declarationNameOf = case _ of
  CST.DeclValue { name: CST.Name { name: CST.Ident n } } -> Just n
  CST.DeclSignature (CST.Labeled { label: CST.Name { name: CST.Ident n } }) -> Just n
  _ -> Nothing

moduleNameOf :: CST.Module Void -> String
moduleNameOf (CST.Module { header: CST.ModuleHeader { name } }) =
  case name of
    CST.Name { name: CST.ModuleName n } -> n

rewriteDecls :: LintContext -> CST.Module Void -> PerDeclaration -> RuleOutcome (CST.Module Void)
rewriteDecls context (CST.Module moduleFields) perDeclaration =
  let
    CST.ModuleBody bodyFields = moduleFields.body
    declResults = map
      (\decl -> perDeclaration (context { declarationName = declarationNameOf decl }) decl)
      bodyFields.decls
  in
    { result: CST.Module
        (moduleFields { body = CST.ModuleBody (bodyFields { decls = map _.result declResults }) })
    , fixed: Array.any _.fixed declResults
    , violations: Array.concatMap _.violations declResults
    }

type ExprLintState = { violations :: Array Finding, fixed :: Boolean }

lintExprsInDecl
  :: Array (Grouped ExprRule)
  -> LintContext
  -> CST.Declaration Void
  -> RuleOutcome (CST.Declaration Void)
lintExprsInDecl exprRules context decl =
  let
    applyExprRules :: CST.Expr Void -> State ExprLintState (CST.Expr Void)
    applyExprRules expr = do
      let exprResult = runRules context exprRules expr
      modify_ \s -> s
        { violations = s.violations <> exprResult.violations
        , fixed = s.fixed || exprResult.fixed
        }
      pure exprResult.result

    Tuple result finalState =
      runState (rewriteDeclBottomUpM (defaultVisitorM { onExpr = applyExprRules }) decl)
        { violations: [], fixed: false }
  in
    { result, fixed: finalState.fixed, violations: finalState.violations }
