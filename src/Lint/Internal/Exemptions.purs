module Lint.Internal.Exemptions
  ( Exempt
  , Exemptions
  , decodeExemptions
  , exemptFile
  , matches
  , noExemptions
  , readExemptions
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (any, filter) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe (isJust, maybe) as Maybe
import Data.String (Pattern(..)) as Str
import Data.String (split, stripPrefix, stripSuffix) as Str
import Effect.Aff (Aff)
import Effect.Aff (attempt) as Aff
import Data.Json.Decode
  ( DecodeJson
  , decodeArray
  , decodeString
  , printJsonDecodeError
  , runDecode
  )
import Data.Json.Decode.Record (decodeRecordWithDefaults)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS

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
type Exempt =
  { rule :: String
  , modules :: Array String
  , paths :: Array String
  , why :: String
  }

type Exemptions = Array Exempt

-- | Where the file lives, relative to the repository being linted.
-- |
-- | Named for exactly what it holds, and not `lint.json`, because the
-- | rule set is a program and must stay one. Thresholds, composition,
-- | phases and custom rules want types and functions; a JSON schema
-- | would be a worse language for them. Exemptions are the one part
-- | with no logic worth typing - a name, a pattern, a reason - and the
-- | one part that has to be writable without depending on the engine.
exemptFile :: String
exemptFile = "lint-exemptions.json"

-- | No file, no exemptions - which is a repository holding itself to
-- | the whole set, not a broken one.
noExemptions :: Exemptions
noExemptions = []

-- | Read them from the working directory. Uses `decodeExemptions`.
-- |
-- | Absent is fine and silent. Present but unreadable is not: a
-- | mistyped exemption file that quietly exempted nothing would be
-- | found by a rule firing somewhere nobody expected, months later.
-- | Uses `decodeExemptions`.
readExemptions :: Aff (Either String Exemptions)
readExemptions = do
  attempted <- Aff.attempt (FS.readTextFile UTF8 exemptFile)
  case attempted of
    Left _ -> pure (Right noExemptions)
    Right text -> pure (decodeExemptions text)

-- | The file's contents, decoded.
decodeExemptions :: String -> Either String Exemptions
decodeExemptions text = case jsonParser text of
  Left err -> Left (exemptFile <> ": " <> err)
  Right json -> case runDecode decodeTop json of
    Left err -> Left (exemptFile <> ": " <> printJsonDecodeError err)
    Right top -> Right top.exempt

-- |
-- | without naming both, and `rule` defaults to `"*"` because the
-- | common case is a module nothing should look at.
-- | Private.
decodeTop :: DecodeJson { exempt :: Exemptions }
decodeTop = decodeRecordWithDefaults { exempt: [] }
  { exempt: decodeArray decodeExempt }

-- | Private.
decodeExempt :: DecodeJson Exempt
decodeExempt = decodeRecordWithDefaults
  { rule: "*", modules: [], paths: [], why: "" }
  { rule: decodeString
  , modules: decodeArray decodeString
  , paths: decodeArray decodeString
  , why: decodeString
  }

-- | Whether a rule is exempt here. Uses `matchesModule`, `matchesPath`.
-- | Uses `covers`, `forRule`.
matches
  :: Exemptions
  -> { rule :: String, moduleName :: String, path :: String, declarationName :: Maybe String }
  -> Boolean
matches exemptions subject =
  Array.any (covers subject) (Array.filter (forRule subject.rule) exemptions)

-- | Private. Used only by `matches`.
forRule :: String -> Exempt -> Boolean
forRule rule one = one.rule == "*" || one.rule == rule

-- | Private. Used only by `matches`. Uses `matchesModule`, `matchesPath`.
covers
  :: { rule :: String, moduleName :: String, path :: String, declarationName :: Maybe String }
  -> Exempt
  -> Boolean
covers subject one =
  Array.any (matchesModule subject) one.modules
    || Array.any (matchesPath subject.path) one.paths

-- | Private, depth 2. Used only by `covers`. Uses `matchesName`.
matchesModule
  :: { rule :: String, moduleName :: String, path :: String, declarationName :: Maybe String }
  -> String
  -> Boolean
matchesModule subject entry = case Str.split (Str.Pattern "#") entry of
  [ modulePattern ] -> matchesName subject.moduleName modulePattern
  [ modulePattern, declName ] ->
    matchesName subject.moduleName modulePattern
      && subject.declarationName == Just declName
  _ -> false

-- |
-- | A trailing `*` is a prefix match and the only wildcard there is.
-- | Anything more would be a glob language nobody asked for.
-- | Private, depth 3. Used only by `matchesModule`.
matchesName :: String -> String -> Boolean
matchesName actual pattern =
  Maybe.maybe (actual == pattern) (\prefix -> Maybe.isJust (Str.stripPrefix (Str.Pattern prefix) actual))
    (Str.stripSuffix (Str.Pattern "*") pattern)

-- |
-- | Matches the end of the path, so `Scratch.purs` covers every module
-- | of that name without naming the package it is in.
-- | Private, depth 2. Used only by `covers`.
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
