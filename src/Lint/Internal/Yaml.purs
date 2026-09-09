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
-- Untyped on the way out on purpose. What comes back is `Json` and
-- every field is decoded by the same decoder that read the JSON form,
-- so the format changed and the schema did not.
