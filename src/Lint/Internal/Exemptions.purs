module Lint.Internal.Exemptions
  ( Exempt
  , Exemptions
  , Kind(..)
  , Origin
  , ExemptionQuery
  , Standing(..)
  , backlogCovering
  , decodeExemptions
  , decodeExemptionsFrom
  , exemptFile
  , matches
  , noExemptions
  , readExemptionsWith
  , rootOrigin
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array (any, concat, filter, find, mapMaybe, null) as Array
import Data.Either (Either(..))
import Data.Either (either) as Either
import Data.Json.Decode
  ( DecodeJson
  , JsonDecodeError(..)
  , decodeArray
  , decodeRawJson
  , decodeRefine
  , decodeString
  , printJsonDecodeError
  , runDecode
  )
import Data.Json.Decode.Record (decodeRecordWithDefaults)
import Data.Maybe (Maybe(..))
import Data.Maybe (isJust, maybe) as Maybe
import Data.String (Pattern(..), split, stripPrefix, stripSuffix) as Str
import Data.Traversable (for, sequence)
import Effect.Aff (Aff)
import Effect.Aff (attempt) as Aff
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Lint.Internal.Yaml (parse) as Yaml
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile) as FS
import Node.FS.Sync (exists) as Sync
import Node.Path (FilePath)
import Node.Path (concat, dirname) as Path

-- | One claim that a rule does not apply somewhere, and why.
-- |
-- | `rule` is a rule's name, or `"*"` for every rule - which is how a
-- | module is skipped wholesale rather than per rule.
-- |
-- | `modules` matches a module name exactly, or by prefix with a
-- | trailing `*`, or one declaration with `Module.Name#declName`.
-- | `paths` matches the end of a file path, so `Scratch.purs` covers
-- | every module of that name in every package.
-- |
-- | `why` is not decoration. An exemption without a reason is
-- | indistinguishable from an oversight six months later, and the
-- | reason is the only part that makes it reviewable.
-- |
-- | `since` is the day it was written, as `YYYY-MM-DD`. It is empty
-- | when nobody said, which is a fact worth reporting rather than a
-- | value worth inventing.
-- |
-- | `file` is where it was read from, and `package` the package whose
-- | directory that file sits in - `Nothing` for the workspace root. A
-- | package's file reaches that package's modules and nothing else, so
-- | `modules: ["*"]` written beside a `spago.yaml` means that package
-- | and does not quietly widen to the workspace.
type Exempt =
  { rule :: String
  , modules :: Array String
  , paths :: Array String
  , kind :: Kind
  , since :: String
  , why :: String
  , file :: FilePath
  , package :: Maybe String
  }

-- | **A `since` and not an `until`**, which was the other proposal and
-- | is worse three ways.
-- |
-- | An `until` is a guess made at the moment you know least - when the
-- | exemption is written, before anyone has tried to remove it. A
-- | `since` is a fact, and it cannot be wrong.
-- |
-- | An `until` that arrives turns the build red on a date nobody chose
-- | for a reason nobody had that morning, so it gets bumped, and a
-- | date that is always bumped is a date that means nothing.
-- |
-- | And age is derivable from a `since`, so anything an `until` was
-- | meant to give is still there: sorting by age ranks exemptions the
-- | same way, and lets a fixer pick the oldest rather than the first
-- | `n` it happens to read. That was the ask this field came from.
-- |
-- | Empty when nobody said. A tool reports those separately rather
-- | than inventing a day, because "nobody knows how old this is" is
-- | itself the finding.
-- |
-- | Whether this is a decision or a debt.
-- |
-- | `ByDesign` is a rule that does not apply here and never will, and
-- | a robot must not undo it. `Backlog` is work nobody has done yet,
-- | which is what a fixer is for.
-- |
-- | Its own field rather than two lists or a word shouted at the front
-- | of the reason. `BACKLOG (31 findings):` was both: a field written
-- | as prose, carrying a count that is wrong the moment anybody fixes
-- | one of them.
data Kind
  = ByDesign
  | Backlog

derive instance Eq Kind

type Exemptions = Array Exempt

-- | Which exemptions a run honours.
-- |
-- | `All` is every run a person does: both lists suppress, so the
-- | build is about the code someone is writing rather than about a
-- | backlog they did not create.
-- |
-- | `ByDesignOnly` is the fixer's view. `pending` stops suppressing,
-- | so the backlog reappears as findings and something can be done
-- | about it - while `exempt` still holds, because those are decisions
-- | rather than debt and a robot must not undo them.
data Standing
  = All
  | ByDesignOnly

derive instance Eq Standing

-- | Where a file of exemptions was read from: its path, and the package
-- | it belongs to when it sits in one.
type Origin = { file :: FilePath, package :: Maybe String }

-- | The one place a rule is asked about: which rule, where, and in
-- | which package. Survey rules pass `""` for a path and, for a
-- | namespace, for the package too - a namespace belongs to nobody, so
-- | only the root file can speak for it.
type ExemptionQuery =
  { rule :: String
  , packageName :: String
  , moduleName :: String
  , path :: String
  , declarationName :: Maybe String
  }

-- | The file's name, in whichever directory it is read from.
-- |
-- | Named for exactly what it holds, and not `lint.json`, because the
-- | rule set is a program and must stay one. Thresholds, composition,
-- | phases and custom rules want types and functions; a JSON schema
-- | would be a worse language for them. Exemptions are the one part
-- | with no logic worth typing - a name, a pattern, a reason - and the
-- | one part that has to be writable without depending on the engine.
exemptFile :: String
exemptFile = "lint-exemptions.yaml"

-- | What it used to be called. Found, it is refused by name, never read.
-- | Private.
oldExemptFile :: String
oldExemptFile = "lint-exemptions.json"

-- | The workspace root's file: everything it says reaches every package.
rootOrigin :: Origin
rootOrigin = { file: exemptFile, package: Nothing }

-- | No file, no exemptions - which is a repository holding itself to
-- | the whole set, not a broken one.
noExemptions :: Exemptions
noExemptions = []

-- | Read every file the workspace has: the root's, and one beside each
-- | package's `spago.yaml`, honouring only the standing asked for.
-- |
-- | Absent is fine and silent. Present but unreadable is not: a
-- | mistyped exemption file that quietly exempted nothing would be
-- | found by a rule firing somewhere nobody expected, months later.
-- | The complaint names the file it is about, which with several files
-- | is the half of the message that matters.
-- |
-- | A package at the root (`path: .`) has no file of its own: the root
-- | file is it, and it is read once, unscoped.
-- | Uses `readOne`, `packageOrigin`.
readExemptionsWith
  :: Standing -> Array { name :: String, path :: FilePath } -> Aff (Either String Exemptions)
readExemptionsWith standing packages = do
  let
    origins :: Array Origin
    origins = [ rootOrigin ] <> Array.mapMaybe packageOrigin packages
  (each :: Array (Either String Exemptions)) <- for origins (readOne standing)
  pure (map Array.concat (sequence each))

-- | Private. Used only by `readExemptionsWith`.
packageOrigin :: { name :: String, path :: FilePath } -> Maybe Origin
packageOrigin { name, path } =
  let
    trimmed :: String
    trimmed = Maybe.maybe path identity (Str.stripPrefix (Str.Pattern "./") path)

    dir :: String
    dir = Maybe.maybe trimmed identity (Str.stripSuffix (Str.Pattern "/") trimmed)
  in
    if dir == "" || dir == "." then Nothing
    else Just { file: Path.concat [ dir, exemptFile ], package: Just name }

-- | One directory's file. The old name beside it, `lint-exemptions.json`,
-- | is refused rather than read: a file under a name the reader has
-- | stopped looking for is a file that exempts nothing and says so to
-- | nobody, which is how three repositories lost theirs on the way to
-- | `.yaml`.
-- | Private. Used only by `readExemptionsWith`. Uses `decodeExemptionsFrom`.
readOne :: Standing -> Origin -> Aff (Either String Exemptions)
readOne standing origin = do
  let
    oldName :: FilePath
    oldName = Path.concat [ Path.dirname origin.file, oldExemptFile ]
  (older :: Boolean) <- liftEffect (Sync.exists oldName)
  (attempted :: Either Error String) <- Aff.attempt (FS.readTextFile UTF8 origin.file)
  pure
    if older then Left (oldName <> ": rename to " <> exemptFile <> ", key `exemptions`")
    else Either.either (const (Right noExemptions)) (decodeExemptionsFrom standing origin) attempted

-- | The root file's contents, decoded, with both kinds honoured.
-- | Uses `decodeExemptionsFrom`.
decodeExemptions :: String -> Either String Exemptions
decodeExemptions = decodeExemptionsFrom All rootOrigin

-- | One file's contents, decoded, honouring one standing, and every
-- | entry stamped with where it came from.
-- | Uses `fromFile`, `decodeTop`.
decodeExemptionsFrom :: Standing -> Origin -> String -> Either String Exemptions
decodeExemptionsFrom standing origin text = do
  (json :: Json) <- fromFile origin identity (Yaml.parse text)
  (top :: { exemptions :: Array Written, exempt :: Array Unit }) <-
    fromFile origin printJsonDecodeError (runDecode decodeTop json)
  (entries :: Array Written) <-
    if Array.null top.exempt then Right top.exemptions
    else Left (origin.file <> ": the key is `exemptions`; `exempt` was the old file's and is read by nothing")
  let
    stamped :: Exemptions
    stamped = map (stamp origin) entries
  pure case standing of
    All -> stamped
    ByDesignOnly -> Array.filter (\(one :: Exempt) -> one.kind == ByDesign) stamped

-- | The same answer, with the file's name in front of whatever went
-- | wrong. Written with `either` rather than a `case`, because a case
-- | that rebuilds `Left` around its own payload and hands `Right`
-- | back untouched is `either` spelled with more syntax.
-- | Private. Used only by `decodeExemptionsFrom`.
fromFile :: ∀ e a. Origin -> (e -> String) -> Either e a -> Either String a
fromFile origin say = Either.either (\(err :: e) -> Left (origin.file <> ": " <> say err)) Right

-- | What one entry says in the file; where it was read from is added
-- | by whoever read it.
-- | Private.
type Written =
  { rule :: String
  , modules :: Array String
  , paths :: Array String
  , kind :: Kind
  , since :: String
  , why :: String
  }

-- | Private. Used only by `decodeExemptionsFrom`.
stamp :: Origin -> Written -> Exempt
stamp origin one =
  { rule: one.rule
  , modules: one.modules
  , paths: one.paths
  , kind: one.kind
  , since: one.since
  , why: one.why
  , file: origin.file
  , package: origin.package
  }

-- | `exempt` is read only to be refused: the old files used it, and a
-- | decoder that ignored it read them as a repository with no
-- | exemptions at all, silently, which is the one thing this file must
-- | never do.
-- | Private.
decodeTop :: DecodeJson { exemptions :: Array Written, exempt :: Array Unit }
decodeTop = decodeRecordWithDefaults { exemptions: [], exempt: [] }
  { exemptions: decodeArray decodeExempt
  , exempt: decodeArray (map (const unit) decodeRawJson)
  }

-- | that does not say whether it is a decision or a debt is the thing
-- | this field was added to stop.
-- | Private.
decodeKind :: DecodeJson Kind
decodeKind = decodeString # decodeRefine \(said :: String) -> case said of
  "by-design" -> Right ByDesign
  "backlog" -> Right Backlog
  _ -> Left (TypeMismatch "kind is `by-design` or `backlog`")

-- | `file` and `package` are not read from the entry: the reader knows
-- | them and stamps them on afterwards, so a file cannot claim to be
-- | another.
-- | Private.
decodeExempt :: DecodeJson Written
decodeExempt = decodeRecordWithDefaults
  { rule: "*"
  , modules: []
  , paths: []
  , kind: Backlog
  , since: ""
  , why: ""
  }
  { rule: decodeString
  , modules: decodeArray decodeString
  , paths: decodeArray decodeString
  , kind: decodeKind
  , since: decodeString
  , why: decodeString
  }

-- | Whether a rule is exempt here. Uses `covers`, `forRule`.
matches :: Exemptions -> ExemptionQuery -> Boolean
matches exemptions subject = Maybe.isJust (covering exemptions subject)

-- | The backlog entry that would have covered this subject, if one
-- | does. A fixer ranks what it attempts by the age of this, so the
-- | oldest debt is tried first rather than whichever finding the walk
-- | happened to meet.
-- | Uses `covering`.
backlogCovering :: Exemptions -> ExemptionQuery -> Maybe Exempt
backlogCovering exemptions = covering (Array.filter (\(one :: Exempt) -> one.kind == Backlog) exemptions)

-- | A root entry speaks for every package; a package's entry only for
-- | its own - that is the `package` test, and it is the whole of what
-- | placing a file beside a `spago.yaml` means.
-- | Private. Used only by `matches`, `backlogCovering`. Uses `matchesModule`, `matchesPath`.
covering :: Exemptions -> ExemptionQuery -> Maybe Exempt
covering exemptions subject =
  let
    forRule :: Exempt -> Boolean
    forRule one = one.rule == "*" || one.rule == subject.rule

    inPackage :: Exempt -> Boolean
    inPackage one = Maybe.maybe true (_ == subject.packageName) one.package

    covers :: Exempt -> Boolean
    covers one =
      Array.any (matchesModule subject) one.modules
        || Array.any (matchesPath subject.path) one.paths
  in
    Array.find covers (Array.filter (\(one :: Exempt) -> forRule one && inPackage one) exemptions)

-- | Private. Used only by `covering`. Uses `matchesName`.
matchesModule :: ExemptionQuery -> String -> Boolean
matchesModule subject entry = case Str.split (Str.Pattern "#") entry of
  [ modulePattern ] -> matchesName subject.moduleName modulePattern
  [ modulePattern, declName ] ->
    matchesName subject.moduleName modulePattern
      && subject.declarationName == Just declName
  _ -> false

-- |
-- | A trailing `*` is a prefix match and the only wildcard there is.
-- | Anything more would be a glob language nobody asked for.
-- | Private, depth 2. Used only by `matchesModule`.
matchesName :: String -> String -> Boolean
matchesName actual pattern =
  Maybe.maybe (actual == pattern) (\(prefix :: String) -> Maybe.isJust (Str.stripPrefix (Str.Pattern prefix) actual))
    (Str.stripSuffix (Str.Pattern "*") pattern)

-- |
-- | Matches the end of the path, so `Scratch.purs` covers every module
-- | of that name without naming the package it is in.
-- | Private. Used only by `covering`.
matchesPath :: String -> String -> Boolean
matchesPath actual entry = Maybe.isJust (Str.stripSuffix (Str.Pattern entry) actual)

-- Context: exemptions are the one part of a lint setup that is a claim
-- about *this* repository rather than about the style, and they were
-- the one part that needed PureScript. Defining one meant importing
-- `Lint.Rule`, which meant depending on the engine, which is why the
-- engine could not be linted by anything but itself.
--
-- Every exemption these repositories actually had reduced to a module
-- pattern, a path suffix and a reason - the `appliesTo` functions were
-- four helpers over a list of strings. So they are data now, and a
-- linter can be pointed at any checkout.
--
-- What that closes: an exemption can no longer be an arbitrary
-- predicate. None was, across three repositories and thirty-five of
-- them, so the door being shut costs nothing today - but it is shut,
-- and the way back is a rule that takes the distinction as
-- configuration rather than an exemption that computes it.
--
-- One file per package, from 2026-10-04. Nineteen repositories became
-- one workspace, and a file at its root would have made every
-- `modules: ["*"]` written for one of them - thirty-seven of those -
-- a claim about all of them. Placement is the scope: an entry says
-- what it always said, and where it sits says how far that reaches.
-- The root file keeps what is genuinely about the workspace: a rule
-- that reports on namespaces, or an entry that spans packages.
