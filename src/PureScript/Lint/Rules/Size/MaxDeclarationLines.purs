module PureScript.Lint.Rules.Size.MaxDeclarationLines (maxDeclarationLines) where

import Prelude

import Data.Maybe (Maybe(..))
import PureScript.CST.Range (rangeOf)
import PureScript.Lint.Rule (DeclarationLint, LintResult(..), RuleName(..))

maxDeclarationLines :: Int -> DeclarationLint
maxDeclarationLines maxLines =
  { name: RuleName "max-declaration-lines"
  , description:
      "Flags a top-level declaration whose own source line span exceeds the configured maximum."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context decl ->
      let
        range = rangeOf decl
        lineSpan = range.end.line - range.start.line + 1
      in
        if lineSpan > maxLines then
          Violation
            ("declaration spans " <> show lineSpan <> " lines, over the max of " <> show maxLines)
        else
          Passed
  }

-- Context: `maxDeclarationLines` flags a top-level declaration whose own
-- source line span exceeds `maxLines` - the one blind spot the
-- nesting/count rules structurally can't catch on their own: a long,
-- flat sequence of statements or a wide chain of `let`/`where` bindings
-- can stay shallow and narrow while still being too big to hold in view
-- at once. Uses `PureScript.CST.Range.rangeOf` directly - no traversal
-- at all, just the declaration's own start/end position, so this is by
-- far the cheapest rule in this family to run.
