module Lint (LintOptions, LintReport, Located, lintWorkspace, runLinter, runLinterWith) where

import Prelude

import Control.Monad.State (State, modify_, runState)
import Data.Array as Array
import Data.Either (Either(..))
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
  , ImportDecl(..)
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
import Effect.Aff (error, throwError) as Aff
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Maybe (fromMaybe, isJust) as Maybe
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Lint.Fix (FixConfig)
import Lint.Fix as Fix
import Lint.Internal.Exemptions (Exemptions)
import Lint.Internal.Exemptions as Exemptions
import Lint.Internal.RuleSet (FlatRules, Rule, flattenRules)
import Lint.Internal.Survey (PackageSurvey, SurveyModule, runSurveyRules)
import Lint.Internal.Workspace (WorkspaceModule)
import Lint.Internal.Workspace as Workspace

-- | Run a rule set over the Spago workspace in the current directory,
-- | reporting everything it finds. `true` when the workspace is clean.
-- |
-- | A rule that rewrites is applied: the module is written back.
runLinter :: Array Rule -> Aff Boolean
runLinter = runLinterWith { skipModules: [], fix: Nothing, standing: Exemptions.All }

-- | `runLinter` with options. Uses `lintWorkspace`.
runLinterWith :: LintOptions -> Array Rule -> Aff Boolean
runLinterWith options rules = do
  case options.fix of
    Nothing -> pure unit
    Just fix -> void (fixWorkspace options fix rules)
  report <- lintWorkspace options rules
  printByRule report.located
  printSummary report.total (Array.length (Array.nub (map _.moduleName report.located)))
    report.moduleCount
  pure (report.total == 0)

-- | What a run found, without printing any of it.
-- |
-- | The same walk `runLinterWith` does, handing back what it saw
-- | instead of a verdict. For anything that has to *act* on findings
-- | rather than show them - which otherwise means reading this
-- | module's own printed output back in, and a tool that parses its own
-- | prose has put the feature in the wrong place.
-- |
-- | `total` counts survey findings too, which have no module and so
-- | never appear in `located`. That is why it is reported rather than
-- | derived from the array's length.
lintWorkspace :: LintOptions -> Array Rule -> Aff LintReport
lintWorkspace { skipModules, standing } rules = do
  exemptions <- readOrFail standing
  workspace <- Workspace.getWorkspace
  let
    flatRules = flattenRules rules
    configured = { skipModules, flatRules, exemptions }
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
  pure
    { located: Array.concatMap _.located scanned
    , total
    , moduleCount
    }

-- | Private. Used only by `lintWorkspace`.
-- |
-- | Absent is fine and means no exemptions. Present but malformed is
-- | not: a mistyped file that quietly exempted nothing would be found
-- | by a rule firing somewhere nobody expected, months later.
readOrFail :: Exemptions.Standing -> Aff Exemptions
readOrFail standing = do
  found <- Exemptions.readExemptionsWith standing
  case found of
    Left why -> Aff.throwError (Aff.error why)
    Right exemptions -> pure exemptions

-- |
-- | Ask for a fix for each finding that has guidance, and keep the ones
-- | that survive being linted again.
-- |
-- | the workspace is linted again *in this process*, and the source is
-- | put back unless the finding is gone and nothing new appeared in
-- | that module. A proposal that does not parse fails that for free,
-- | because linting it has to parse it.
-- |
-- | Putting it back needs no git and no copy on disk. The source was
-- | read to build the brief and is still in hand.
-- |
-- | At most one finding per module, so one run's fixes are independent
-- | of each other: two in a module stack, the second written on top of
-- | the first, and then neither can be looked at alone.
-- | Private, depth 2. Used only by `runLinterWith`. Uses `lintWorkspace`, `sameModule`,
-- | `hasGuidance`, `attemptOne`.
fixWorkspace :: LintOptions -> FixConfig -> Array Rule -> Aff Int
fixWorkspace options fix rules = do
  report <- lintWorkspace options rules
  let
    mine = Array.take fix.limit
      (Array.nubByEq sameModule (Array.filter (hasGuidance fix.guidance) report.located))
  log ""
  log
    ( "  " <> show (Array.length report.located) <> " findings, "
        <> show (Array.length mine)
        <> " with guidance to try"
    )
  fixes <- for mine \one -> do
    outcome <- attemptOne options fix rules report.located one
    log ("  " <> Fix.outcomeLine one.finding.rule.name one.moduleName outcome)
    pure (if outcome == Fix.Fixed then 1 else 0)
  pure (sum fixes)

-- | Private, depth 3. Used only by `fixWorkspace`.
hasGuidance :: Array Fix.Guidance -> Located -> Boolean
hasGuidance table one = Maybe.isJust (Fix.guidanceFor table one.finding.rule.name)

-- | Private, depth 3. Used only by `fixWorkspace`.
sameModule :: Located -> Located -> Boolean
sameModule a b = a.moduleName == b.moduleName

-- | Private, depth 3. Used only by `fixWorkspace`. Uses `attemptRound`.
attemptOne
  :: LintOptions
  -> FixConfig
  -> Array Rule
  -> Array Located
  -> Located
  -> Aff Fix.Outcome
attemptOne options fix rules before one = do
  was <- FS.readTextFile UTF8 one.path
  tailRecM (attemptRound options fix rules before one was) { left: fix.rounds, broke: [] }

-- | Private, depth 4. Used only by `attemptOne`. Uses `judge`.
attemptRound
  :: LintOptions
  -> FixConfig
  -> Array Rule
  -> Array Located
  -> Located
  -> String
  -> { left :: Int, broke :: Array String }
  -> Aff (Step { left :: Int, broke :: Array String } Fix.Outcome)
attemptRound options fix rules before one was state = do
  proposed <- fix.propose
    { rule: one.finding.rule.name
    , moduleName: one.moduleName
    , path: one.path
    , message: one.finding.message
    , guidance: Maybe.fromMaybe "" (Fix.guidanceFor fix.guidance one.finding.rule.name)
    , source: was
    , broke: state.broke
    }
  case proposed of
    Left why -> pure (Done (Fix.Declined why))
    Right text -> do
      judged <- judge options fix rules before one was text
      if state.left > 1 && not (Array.null judged.broke) then
        pure (Loop { left: state.left - 1, broke: judged.broke })
      else pure (Done judged.outcome)

-- | Private, depth 5. Used only by `attemptRound`. Uses `lintWorkspace`, `same`,
-- | `describe`, `verified`.
-- |
-- | Two gates, cheapest first. The re-lint is a parse of the workspace;
-- | `fix.verify` is usually a compile, so it runs only for a proposal
-- | that has already earned it.
judge
  :: LintOptions
  -> Fix.FixConfig
  -> Array Rule
  -> Array Located
  -> Located
  -> String
  -> String
  -> Aff { outcome :: Fix.Outcome, broke :: Array String }
judge options fix rules before one was text = do
  FS.writeTextFile UTF8 one.path text
  after <- lintWorkspace options rules
  let here = Array.filter (\a -> a.moduleName == one.moduleName) after.located
  let wasHere = Array.filter (\a -> a.moduleName == one.moduleName) before
  let started = Array.filter (\a -> not (Array.any (same a) wasHere)) here
  if Array.any (same one) here then do
    FS.writeTextFile UTF8 one.path was
    pure { outcome: Fix.Declined "the finding is still there", broke: [] }
  else if not (Array.null started) then do
    FS.writeTextFile UTF8 one.path was
    pure
      { outcome: Fix.Declined ("it introduced " <> newFindings started)
      , broke: map describe started
      }
  else do
    built <- verified fix
    case built of
      Left why -> do
        FS.writeTextFile UTF8 one.path was
        pure { outcome: Fix.Declined "it does not compile", broke: [ why ] }
      Right _ -> pure { outcome: Fix.Fixed, broke: [] }

-- | Private, depth 6. Used only by `judge`.
-- |
-- | No `verify` configured means nothing to fail, not nothing to run.
verified :: Fix.FixConfig -> Aff (Either String Unit)
verified fix = case fix.verify of
  Nothing -> pure (Right unit)
  Just check -> check

-- | Private, depth 6. Used only by `judge`. Uses `describe`.
-- |
-- | Names the rules rather than counting them. This line is what a
-- | person reads to decide whether a proposal was close or nowhere
-- | near, and two rules that answer each other show up here as a pair -
-- | which a count hides.
newFindings :: Array Located -> String
newFindings started = case started of
  [ only ] -> "a new finding, " <> describe only
  _ ->
    show (Array.length started) <> " new findings: "
      <> String.joinWith ", " (Array.nub (map (\a -> a.finding.rule.name) started))

-- | Private, depth 6. Used only by `judge`.
describe :: Located -> String
describe one = one.finding.rule.name <> ": " <> one.finding.message

-- | Private, depth 6. Used only by `judge`.
same :: Located -> Located -> Boolean
same a b =
  a.moduleName == b.moduleName
    && a.finding.rule.name == b.finding.rule.name
    && a.finding.message == b.finding.message

reportSurvey :: Configured -> Array PackageSurvey -> Aff Int
reportSurvey { flatRules, exemptions } surveys = do
  let
    forPackage s = map (append (s.packageName <> ": "))
      (runSurveyRules exemptions flatRules.packages s)
    perPackage = Array.concatMap forPackage surveys
    perWorkspace = runSurveyRules exemptions flatRules.workspaces { packages: surveys }
    findings = perPackage <> perWorkspace
  for_ findings \msg -> log ("  " <> msg)
  pure (Array.length findings)

-- | `skipModules` names modules no rule should see at all.
-- | Everything a run is configured with.
-- |
-- | `skipModules` names modules no rule should see at all.
-- |
-- | `fix` turns effect-resolved fixes on, and `Nothing` is how they
-- | stay off. It carries the effect itself - a function from a brief to
-- | new source - so what performs it is decided here, from outside, and
-- | nothing in this package can find out what it got.
type LintOptions =
  { skipModules :: Array ModuleExemption
  , fix :: Maybe FixConfig
  -- | Which exemptions this run honours. `All` for a person's run;
  -- | `ByDesignOnly` for a fixer, which is the one caller that should
  -- | see the pending backlog rather than have it suppressed.
  , standing :: Exemptions.Standing
  }

-- | Everything one run saw.
type LintReport =
  { located :: Array Located
  , total :: Int
  , moduleCount :: Int
  }

type Configured =
  { skipModules :: Array ModuleExemption
  , flatRules :: FlatRules
  , exemptions :: Exemptions
  }

type ModuleScan =
  { surveyed :: SurveyModule
  , violations :: Int
  , located :: Array Located
  }

-- | One finding, and the module it was found in.
type Located = { moduleName :: String, path :: String, finding :: Finding }

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
lintModule { skipModules, flatRules, exemptions } packageName workspaceModule = do
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
      { moduleName: context.moduleName
      , path: context.path
      , kind: context.kind
      , imports: importsOf original
      }
  if Array.any (\g -> g.appliesTo context) skipModules then
    pure { surveyed, violations: 0, located: [] }
  else do
    let
      afterModules = runRules exemptions context flatRules.modules original
      afterDeclarations = rewriteDecls context afterModules.result
        (\ctx -> runRules exemptions ctx flatRules.declarations)
      afterExpressions = rewriteDecls context afterDeclarations.result
        (lintExprsInDecl exemptions flatRules.expressions)
      violations = afterModules.violations <> afterDeclarations.violations <>
        afterExpressions.violations
      fixed = afterModules.fixed || afterDeclarations.fixed || afterExpressions.fixed
    pure unit
    when fixed (Workspace.writeModule workspaceModule.path afterExpressions.result)
    pure
      { surveyed
      , violations: Array.length violations
      , located: map
          (\f -> { moduleName: context.moduleName, path: context.path, finding: f })
          violations
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

-- | Every module this one imports, by name.
-- |
-- | On the survey rather than only in the parse tree, because the
-- | questions worth asking about imports are about the *graph* - how
-- | deep the chain under a module goes - and a rule handed one module at
-- | a time can count its imports but never follow one.
importsOf :: CST.Module Void -> Array String
importsOf (CST.Module { header: CST.ModuleHeader { imports } }) =
  map
    ( \(CST.ImportDecl { module: CST.Name { name: CST.ModuleName n } }) -> n
    )
    imports

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
  :: Exemptions
  -> Array (Grouped ExprRule)
  -> LintContext
  -> CST.Declaration Void
  -> RuleOutcome (CST.Declaration Void)
lintExprsInDecl exemptions exprRules context decl =
  let
    applyExprRules :: CST.Expr Void -> State ExprLintState (CST.Expr Void)
    applyExprRules expr = do
      let exprResult = runRules exemptions context exprRules expr
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
