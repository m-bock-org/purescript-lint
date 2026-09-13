module Lint.Internal.Yaml
  ( parse
  ) where

import Data.Argonaut.Core (Json)
import Data.Either (Either(..))

-- | One YAML document as `Json`, or the parser's sentence about why
-- | not.
-- |
-- | JSON is YAML, which is the whole reason this is the only parser:
-- | a file written either way reads the same, so nothing has to be
-- | converted and no format has a flag day.
parse :: String -> Either String Json
parse text =
  let
    read = parseImpl text
  in
    if read.ok then Right read.value else Left read.why

foreign import parseImpl :: String -> { ok :: Boolean, value :: Json, why :: String }

-- Context: `js-yaml` rather than a parser of our own. YAML is a large
-- specification with several ways to be surprising, and the surprising
-- parts are exactly the ones a hand-written subset gets wrong quietly -
-- an unquoted `no` that becomes `false`, an indented block that folds
-- differently than it reads.
--
-- `CORE_SCHEMA`, which is js-yaml's default minus timestamps. With the
-- default, an unquoted `2026-09-13` comes back as a `Date` - not JSON,
-- so `Json` was a claim this module did not keep, and a decoder asking
-- for a string got "Expected value of type 'String'" with no line
-- number. A configuration file has no business carrying typed dates.
-- The result then goes through `JSON.parse(JSON.stringify(...))`, so
-- what comes back is JSON however js-yaml is configured next.
--
-- Untyped on the way out on purpose. What comes back is `Json` and
-- every field is decoded by the same decoder that read the JSON form,
-- so the format changed and the schema did not.
