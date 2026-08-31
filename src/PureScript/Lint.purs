module PureScript.Lint (LintOptions, runLinter, runLinterWith) where

import Prelude

import Control.Monad.State (State, modify_, runState)
import Data.Array as Array
import Data.Foldable (fold, for_, sum)
import Data.Maybe (Maybe(..))
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
import PureScript.Lint.Internal.Rule (ExprRule, LintContext, ModuleExemption, RuleOutcome, runRules)
import PureScript.Lint.Internal.RuleSet (FlatRules, Rule, flattenRules)
import PureScript.Lint.Internal.Survey (PackageSurvey, SurveyModule, runSurveyRules)
import PureScript.Lint.Internal.Workspace (Workspace, WorkspaceModule)
import PureScript.Lint.Internal.Workspace as Workspace

runLinter :: Array Rule -> Aff Int
runLinter = runLinterWith { skipModules: [] }

runLinterWith :: LintOptions -> Array Rule -> Aff Int
runLinterWith { skipModules } rules = do
  workspace <- Workspace.getWorkspace
  printWorkspace workspace
  let
    flatRules = flattenRules rules
    configured = { skipModules, flatRules }
  log $ fold [ "Linter: ", show (ruleCount flatRules), " rule(s)" ]
  scanned <- for workspace.packages \pkg -> do
    perModule <- for pkg.modules (lintModule configured pkg.name)
    let survey = { packageName: pkg.name, packagePath: pkg.path, modules: map _.surveyed perModule }
    pure { survey, violations: sum (map _.violations perModule) }
  surveyed <- reportSurvey configured (map _.survey scanned)
  let total = sum (map _.violations scanned) + surveyed
  log ("Linter: " <> show total <> " violation(s)")
  pure total

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

ruleCount :: FlatRules -> Int
ruleCount flatRules = Array.length flatRules.modules
  + Array.length flatRules.declarations
  + Array.length flatRules.expressions
  + Array.length flatRules.packages
  + Array.length flatRules.workspaces

-- | `skipModules` names modules no rule should see at all.
type LintOptions = { skipModules :: Array ModuleExemption }

type Configured =
  { skipModules :: Array ModuleExemption
  , flatRules :: FlatRules
  }

type ModuleScan = { surveyed :: SurveyModule, violations :: Int }

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
    pure { surveyed, violations: 0 }
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
    for_ violations \msg -> log ("  " <> workspaceModule.path <> ": " <> msg)
    when fixed (Workspace.writeModule workspaceModule.path afterExpressions.result)
    pure { surveyed, violations: Array.length violations }

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

type ExprLintState = { violations :: Array String, fixed :: Boolean }

lintExprsInDecl :: Array ExprRule -> LintContext -> CST.Declaration Void -> RuleOutcome (CST.Declaration Void)
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

printWorkspace :: Workspace -> Aff Unit
printWorkspace { packages } = do
  log $ fold
    [ "Linter: "
    , show (Array.length packages)
    , " local packages, "
    , show $ Array.length $ Array.concatMap _.modules packages
    , " modules"
    ]
  for_ packages \{ name, modules } -> do
    log $ fold [ "  ", name, " (", show (Array.length modules), ")" ]
    for_ modules \{ path } -> log ("    " <> path)
