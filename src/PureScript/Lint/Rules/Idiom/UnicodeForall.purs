module PureScript.Lint.Rules.Idiom.UnicodeForall (unicodeForall) where

import Prelude

import Data.Array (null) as Array
import Data.Maybe (Maybe(..))
import PureScript.CST.Traversal
  ( defaultMonoidalVisitor
  , defaultVisitor
  , foldMapDecl
  , rewriteDeclBottomUp
  )
import PureScript.CST.Types (Declaration, SourceStyle(..), SourceToken, Token(..), Type(..))
import PureScript.Lint.Rule (DeclarationLint, LintResult(..), RuleName(..))

-- | Uses `hasAsciiForall`, `toUnicode`.
unicodeForall :: DeclarationLint
unicodeForall =
  { name: RuleName "unicode-forall"
  , description: "Requires the unicode forall quantifier rather than the ASCII keyword."
  , goodExample: Just "identity :: \x2200 a. a -> a"
  , badExample: Just "identity :: forall a. a -> a"
  , rule: \_context decl ->
      if hasAsciiForall decl then Fixed (toUnicode decl) else Passed
  }

-- | Private. Used only by `unicodeForall`. Uses `asciiForalls`.
hasAsciiForall :: Declaration Void -> Boolean
hasAsciiForall decl = not (Array.null (asciiForalls decl))

-- | Private, depth 2. Used only by `hasAsciiForall`. Uses `isAscii`.
asciiForalls :: Declaration Void -> Array Unit
asciiForalls = foldMapDecl
  ( defaultMonoidalVisitor
      { onType = case _ of
          TypeForall token _ _ _ -> if isAscii token then [ unit ] else []
          _ -> []
      }
  )

-- | Private. Used only by `unicodeForall`. Uses `unicodeToken`.
toUnicode :: Declaration Void -> Declaration Void
toUnicode = rewriteDeclBottomUp
  ( defaultVisitor
      { onType = case _ of
          TypeForall token bindings dot inner ->
            TypeForall (unicodeToken token) bindings dot inner
          other -> other
      }
  )

-- | Private, depth 3. Used only by `asciiForalls`.
isAscii :: SourceToken -> Boolean
isAscii token = token.value == TokForall ASCII

-- | Private, depth 2. Used only by `toUnicode`.
unicodeToken :: SourceToken -> SourceToken
unicodeToken token = token { value = TokForall Unicode }

-- ## Context
--
-- `∀` over `forall`. The quantifier is punctuation, not a word: it says
-- "for every choice of the following" and is read at a glance once you
-- know it, the way `->` is. Spelling it out gives six characters of
-- ordinary-looking text the same visual weight as the type it prefixes,
-- and every signature in the codebase starts with it, so it is the one
-- token most often skipped and least often worth reading.
--
-- Auto-fixing, because the change is total and mechanical: the CST
-- carries the choice as `TokForall ASCII` vs `TokForall Unicode` on a
-- single token, `PureScript.CST.printModule` renders whichever it
-- finds, and no other part of the declaration is touched. There is no
-- judgment for a human to apply, which is the bar this codebase sets
-- for a fixer.
--
-- Fixing at the token rather than the text is what makes it safe. A
-- search-and-replace for the word would also hit `forall` inside a
-- string literal, a comment, or an identifier that happens to contain
-- it; the parser has already decided which occurrences are the
-- quantifier.
--
-- Only `TypeForall` is rewritten. That is the sole place the keyword
-- can appear in this CST - class and instance heads carry their
-- constraints without one - so there is no second site to keep in sync.
