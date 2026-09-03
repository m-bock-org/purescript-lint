-- | The exemption file, read end to end.
-- |
-- | Not a unit test of the matcher: the question worth answering is
-- | whether a file sitting in a repository actually silences a rule,
-- | and only a real run answers that.
module Test.Lint.ExemptionsSpec (spec) where

import Prelude

import Data.Array (filter, length) as Array
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Lint (lintWorkspace)
import Lint.Internal.Exemptions (decodeExemptions, exemptFile, matches)
import Lint.Rule (perDecl)
import Lint.RuleSet (Rule)
import Lint.RuleSet as RuleSet
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Test.Lint.ReadmeExample (maxFunctionArity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "the exemption file" do
    it "silences a rule where it says to" do
      without <- countFindings
      writeFileWith """
        { "exempt":
          [ { "rule": "max-function-arity"
            , "modules": ["Lint.Internal.*"]
            , "why": "proving the file is read"
            }
          ]
        }
      """
      with <- countFindings
      FS.unlink exemptFile
      -- Fewer, not zero: the exemption names one rule in one
      -- namespace, so everything outside it must still be found.
      (with < without) `shouldEqual` true
      (with > 0) `shouldEqual` true

    it "and an absent file exempts nothing" do
      before <- countFindings
      after <- countFindings
      after `shouldEqual` before

  describe "matching" do
    it "takes a trailing star as a prefix" do
      matches sample { rule: "r", moduleName: "A.B.C", path: "", declarationName: Nothing }
        `shouldEqual` true
    it "and does not match a different prefix" do
      matches sample { rule: "r", moduleName: "X.Y", path: "", declarationName: Nothing }
        `shouldEqual` false
    it "matches a path by its end, so a package need not be named" do
      matches byPath
        { rule: "r", moduleName: "Any", path: "purs/deep/Scratch.purs", declarationName: Nothing }
        `shouldEqual` true
    it "a rule of * covers every rule" do
      matches sample { rule: "anything", moduleName: "A.B", path: "", declarationName: Nothing }
        `shouldEqual` true

-- | Private. Used by the specs.
sample :: Array { modules :: Array String, paths :: Array String, rule :: String, why :: String }
sample = [ { rule: "*", modules: [ "A.*" ], paths: [], why: "because" } ]

-- | Private. Used by the specs.
byPath :: Array { modules :: Array String, paths :: Array String, rule :: String, why :: String }
byPath = [ { rule: "*", modules: [], paths: [ "Scratch.purs" ], why: "because" } ]

-- | Private. Used by the specs. Uses `rules`.
countFindings :: Aff Int
countFindings = do
  report <- lintWorkspace { skipModules: [] } rules
  pure (Array.length (Array.filter inInternal report.located))

-- | Private. Used only by `countFindings`.
inInternal :: forall r. { moduleName :: String | r } -> Boolean
inInternal _ = true

-- | Private. Used by the specs.
writeFileWith :: String -> Aff Unit
writeFileWith text = FS.writeTextFile UTF8 exemptFile text

-- | Private. Used by `countFindings`.
rules :: Array Rule
rules = [ RuleSet.rule (perDecl maxFunctionArity 0) ]
