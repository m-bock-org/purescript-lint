module PureScript.Lint (runLinter) where

import Prelude

import Control.Monad.State (State, modify_, runState)
import Data.Array as Array
import Data.Foldable (fold, for_, sum)
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe) as Maybe
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
import PureScript.Lint.Rule (ExprRule, LintContext, RuleAlias, RuleOutcome, runRules)
import PureScript.Lint.Rule.Survey (PackageSurvey, SurveyModule, runSurveyRules)
import PureScript.Lint.RuleSet (FlatRules, LintPhase, LintRuleSet, flattenRules)
import PureScript.Lint.Workspace (Workspace, WorkspaceModule)
import PureScript.Lint.Workspace as Workspace

-- | Uses `printWorkspace`, `phaseNamed`, `ruleCount`, `lintModule`, `reportSurvey`.
runLinter :: Array RuleAlias -> String -> LintRuleSet -> Aff Int
runLinter excludeRules phaseName ruleSet = do
  workspace <- Workspace.getWorkspace
  printWorkspace workspace
  let
    selected = phaseNamed phaseName ruleSet.phases
    flatRules = flattenRules selected.rules
    configured = { excludeRules, ruleSet, flatRules }
  log $ fold [ "Linter: phase ", selected.name, ", ", show (ruleCount flatRules), " rule(s)" ]
  scanned <- for workspace.packages \pkg -> do
    perModule <- for pkg.modules (lintModule configured pkg.name)
    let survey = { packageName: pkg.name, packagePath: pkg.path, modules: map _.surveyed perModule }
    pure { survey, violations: sum (map _.violations perModule) }
  surveyed <- reportSurvey configured (map _.survey scanned)
  let total = sum (map _.violations scanned) + surveyed
  log ("Linter: " <> show total <> " violation(s)")
  pure total

-- | Private. Used only by `runLinter`.
reportSurvey :: Configured -> Array PackageSurvey -> Aff Int
reportSurvey { excludeRules, flatRules } surveys = do
  let
    forPackage s = map (append (s.packageName <> ": "))
      (runSurveyRules excludeRules flatRules.packages s)
    perPackage = Array.concatMap forPackage surveys
    perWorkspace = runSurveyRules excludeRules flatRules.workspaces { packages: surveys }
    findings = perPackage <> perWorkspace
  for_ findings \msg -> log ("  " <> msg)
  pure (Array.length findings)

-- | Private. Used only by `runLinter`.
phaseNamed :: String -> Array LintPhase -> LintPhase
phaseNamed wanted phases = Maybe.fromMaybe { name: wanted, rules: [] }
  (Array.find (\p -> p.name == wanted) phases)

-- | Private. Used only by `runLinter`.
ruleCount :: FlatRules -> Int
ruleCount flatRules = Array.length flatRules.modules
  + Array.length flatRules.declarations
  + Array.length flatRules.expressions
  + Array.length flatRules.packages
  + Array.length flatRules.workspaces

type Configured =
  { excludeRules :: Array RuleAlias, ruleSet :: LintRuleSet, flatRules :: FlatRules }

type ModuleScan = { surveyed :: SurveyModule, violations :: Int }

-- | Private. Used only by `runLinter`. Uses `moduleNameOf`, `rewriteDecls`, `lintExprsInDecl`.
lintModule :: Configured -> String -> WorkspaceModule -> Aff ModuleScan
lintModule { excludeRules, ruleSet, flatRules } packageName workspaceModule = do
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
  if Array.any (\g -> g.appliesTo context) ruleSet.globalExclude then
    pure { surveyed, violations: 0 }
  else do
    let
      afterModules = runRules { excludeRules, context } flatRules.modules original
      afterDeclarations = rewriteDecls context afterModules.result
        (\ctx -> runRules { excludeRules, context: ctx } flatRules.declarations)
      afterExpressions = rewriteDecls context afterDeclarations.result
        (lintExprsInDecl { excludeRules, exprRules: flatRules.expressions })
      violations = afterModules.violations <> afterDeclarations.violations <>
        afterExpressions.violations
      fixed = afterModules.fixed || afterDeclarations.fixed || afterExpressions.fixed
    for_ violations \msg -> log ("  " <> workspaceModule.path <> ": " <> msg)
    when fixed (Workspace.writeModule workspaceModule.path afterExpressions.result)
    pure { surveyed, violations: Array.length violations }

type PerDeclaration =
  LintContext -> CST.Declaration Void -> RuleOutcome (CST.Declaration Void)

-- | Private, depth 3. Used only by `rewriteDecls`.
declarationNameOf :: CST.Declaration Void -> Maybe String
declarationNameOf = case _ of
  CST.DeclValue { name: CST.Name { name: CST.Ident n } } -> Just n
  CST.DeclSignature (CST.Labeled { label: CST.Name { name: CST.Ident n } }) -> Just n
  _ -> Nothing

-- | Private, depth 2. Used only by `lintModule`.
moduleNameOf :: CST.Module Void -> String
moduleNameOf (CST.Module { header: CST.ModuleHeader { name } }) =
  case name of
    CST.Name { name: CST.ModuleName n } -> n

-- | Private, depth 2. Used only by `lintModule`. Uses `declarationNameOf`.
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

-- | Private, depth 2. Used only by `lintModule`.
type ExprPass = { excludeRules :: Array RuleAlias, exprRules :: Array ExprRule }

-- | Private, depth 2. Used only by `lintModule`.
lintExprsInDecl :: ExprPass -> LintContext -> CST.Declaration Void -> RuleOutcome (CST.Declaration Void)
lintExprsInDecl { excludeRules, exprRules } context decl =
  let
    applyExprRules :: CST.Expr Void -> State ExprLintState (CST.Expr Void)
    applyExprRules expr = do
      let exprResult = runRules { excludeRules, context } exprRules expr
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

-- | Private. Used only by `runLinter`.
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

-- Context: the library's own entry point - a generic PureScript CST
-- linter, not tied to any one repo's rules (see the sibling `lint`
-- package for this repo's own `LintRuleSet`). Meant to eventually
-- move out of the trading-bot repo and stand alone; developed in here
-- for now. runLinter reads the workspace, flattens `ruleSet.lintRules`
-- once via `PureScript.Lint.Rule.flattenRules` (see that module's own
-- trailing comment - a `LintRuleSet` authors one freely-ordered/
-- grouped `Array Rule`, but the runner still needs the three
-- homogeneous arrays this module was already built around), then runs
-- every module/declaration rule over every discovered module. Returns
-- the total violation count; a nonzero count is what `Lint.Main`
-- treats as reason to exit nonzero. `excludeRules` names rules to skip
-- entirely, by their own `name` (see `PureScript.Lint.Rule`) -
-- `Lint.Main`'s `--exclude` CLI flag, so a whole rule (e.g. the
-- comment policy) can be turned off for one run without touching
-- `Lint.purs`.
--
-- lintModule runs a module through the flattened module rules, then
-- the flattened declaration rules over every declaration, then the
-- flattened expression rules over every expression nested anywhere
-- inside each (possibly already-fixed) declaration, splicing any
-- fixed value back in at each level before writing. Returns how many
-- violations this one module had. lintDeclarationsOf threads every
-- declaration in a module through the given declaration rules
-- independently, then rebuilds the module with any fixed declarations
-- spliced back in. lintExpressionsOf threads every expression nested
-- anywhere inside every declaration in a module through the given
-- expression rules - a bottom-up rewrite
-- (`PureScript.CST.Traversal.rewriteDeclBottomUpM`) over each
-- declaration's own expression tree, run in `State` so a rule's
-- violations/fixes accumulate across the whole tree instead of being
-- limited to one expression node in isolation, the way lintModule/
-- lintDeclarationsOf each only ever see one top-level value at a
-- time.
--
-- printWorkspace is written out rather than `show`n: a `Workspace` is
-- a few hundred module paths, and the point of printing it is to
-- check discovery found the right packages and the right files, which
-- one long line of record syntax does not make easy to see.
