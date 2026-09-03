module Lint.Internal.Dedent (dedent) where

import Prelude

import Data.Array (filter, head, init, last, tail) as Array
import Data.Foldable (minimum) as Foldable
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe) as Maybe
import Data.String (Pattern(..)) as Str
import Data.String.CodeUnits (drop, dropWhile, length) as Str
import Data.String.Common (joinWith, split, trim) as Str

-- | Strip a leading and trailing blank line and the shared indentation
-- | from the rest, so an example can be written indented to match the
-- | code around it.
dedent :: String -> String
dedent raw =
  let
    allLines = Str.split (Str.Pattern "\n") raw

    withoutLeadingBlank = case Array.head allLines, Array.tail allLines of
      Just first, Just rest | Str.trim first == "" -> rest
      _, _ -> allLines

    contentLines = case Array.init withoutLeadingBlank, Array.last withoutLeadingBlank of
      Just initLines, Just lastLine | Str.trim lastLine == "" -> initLines
      _, _ -> withoutLeadingBlank

    indentOf line = Str.length line - Str.length (Str.dropWhile (_ == ' ') line)

    nonBlank = Array.filter (\l -> Str.trim l /= "") contentLines
    commonIndent = Maybe.fromMaybe 0 (Foldable.minimum (map indentOf nonBlank))

    stripIndent line
      | Str.trim line == "" = ""
      | otherwise = Str.drop commonIndent line
  in
    Str.joinWith "\n" (map stripIndent contentLines)

-- Context: string handling and nothing else - no rule, no context, no
-- CST. It sat in `Internal.Rule` among the machinery that decides what
-- a rule is and what running one means, where it was the one
-- declaration a reader had to skip past twice: once looking for how
-- rules work, and once looking for how examples are printed.
--
-- Its own module because it is testable on its own, has no dependency
-- on anything in this package, and is the sort of thing that would be
-- a library function somewhere else.
