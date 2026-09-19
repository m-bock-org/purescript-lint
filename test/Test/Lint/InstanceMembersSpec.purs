module Test.Lint.InstanceMembersSpec (spec) where

import Prelude

import Data.Array (mapMaybe) as Array
import Data.Maybe (Maybe(..))
import Data.String (joinWith) as Str
import Lint (rewriteDecls)
import Lint.Internal.Rule
  ( Finding
  , LintContext
  , ModuleKind(..)
  , RuleOutcome
  )
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types (Declaration(..), Module)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | Uses `seenIn`.
spec :: Spec Unit
spec = describe "a declaration rule and the instances in a module" do
  it "is handed a top-level value and its signature, as it always was" do
    seenIn
      [ "top :: Int"
      , "top = 1"
      ] `shouldEqual` [ "top", "top" ]

  it "is handed an instance member as a declaration of its own" do
    seenIn
      [ "instance showThing :: Show Thing where"
      , "  show t = \"thing\""
      ] `shouldEqual` [ "show" ]

  it "is handed a member of every instance in a chain" do
    seenIn
      [ "instance showOne :: Show One where"
      , "  show _ = \"one\""
      , "else instance showTwo :: Show Two where"
      , "  show _ = \"two\""
      ] `shouldEqual` [ "show", "show" ]

  it "is handed a member's signature as a signature" do
    seenIn
      [ "instance showThing :: Show Thing where"
      , "  show :: Thing -> String"
      , "  show _ = \"thing\""
      ] `shouldEqual` [ "show", "show" ]

  it "says nothing about an instance with no body" do
    seenIn [ "instance showThing :: Show Thing" ] `shouldEqual` []

  it "still sees the top-level values around an instance" do
    seenIn
      [ "top :: Int"
      , "top = 1"
      , ""
      , "instance showThing :: Show Thing where"
      , "  show _ = \"thing\""
      ] `shouldEqual` [ "top", "top", "show" ]

-- | Private. Used only by `spec`. Uses `source`, `recorder`.
seenIn :: Array String -> Array String
seenIn body = case parseModule (source body) of
  ParseSucceeded m -> (recorder m).violations # map _.message
  _ -> [ "the fixture did not parse" ]

-- | Private, depth 2. Used only by `seenIn`.
source :: Array String -> String
source body = Str.joinWith "\n" ([ "module M where", "" ] <> body) <> "\n"

-- | Private, depth 2. Used only by `seenIn`. Uses `noted`, `named`.
recorder :: Module Void -> RuleOutcome (Module Void)
recorder m = rewriteDecls topContext m \context decl ->
  { result: decl
  , fixed: false
  , violations: Array.mapMaybe (map noted) [ named decl context ]
  }

-- | Private, depth 3. Used only by `recorder`.
noted :: String -> Finding
noted message =
  { rule: { name: "recorder", description: "", examples: Nothing }
  , groups: []
  , message
  , hint: Nothing
  }

-- | Private, depth 3. Used only by `recorder`.
named :: Declaration Void -> LintContext -> Maybe String
named decl context = case decl of
  DeclValue _ -> context.declarationName
  DeclSignature _ -> context.declarationName
  _ -> Nothing

-- | Private.
topContext :: LintContext
topContext =
  { packageName: "sample-pkg"
  , moduleName: "M"
  , declarationName: Nothing
  , path: "src/M.purs"
  , kind: SourceModule
  }

-- ## Context
--
-- What this pins is that a declaration rule is handed an instance
-- member without knowing it is one, and can find out. Every rule in
-- the regulator matched `DeclValue` and nothing else, so before this
-- change a `where` inside an instance member - or an ignored
-- parameter, or a bare `hush`, or any of the other body-level things
-- rules look for - was invisible to all of them at once.
--
-- The member is handed over as the declaration it would be at the top
-- level: `InstanceBindingName` as `DeclValue`, `InstanceBindingSignature`
-- as `DeclSignature`. Both conversions are total and lossless, because
-- an instance binding carries exactly the fields the declaration does.
-- That is what makes this a change to the engine rather than to
-- twenty-three rules.
--
-- Nothing tells a rule that what it is holding came from an instance,
-- and that was a decision rather than an omission. The obvious design
-- adds a `TopLevel | InstanceMember` to `LintContext` so the rules
-- that are about a name the module chose can opt out - an instance
-- member's name is the class's choice, after all. It was built that
-- way first and then taken out, because the flood it guards against
-- was measured and is not one: across the eleven repositories in this
-- fleet, 80 instance members carry a name on the house's vague-name
-- list, and 73 of them are `show`. `show` is never a name anybody
-- writes at the top level, so it leaves that list instead. The
-- remaining seven are `value`, `parse` and `one`, which is an
-- exemption entry rather than a concept in the engine.
--
-- Worth keeping because the reasoning does not survive the numbers
-- changing: if a later rule set makes the distinction cost something
-- real, the field is the answer and this is where to look for why it
-- is not here yet.
--
-- The chain case is the one that would rot quietly: `else instance`
-- puts the second instance in the separator's tail, and a first
-- attempt that walked only the head would pass every other test here.
