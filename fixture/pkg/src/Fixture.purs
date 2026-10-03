module Fixture where

-- A declaration with a binder, so `max-function-arity` at zero has a
-- finding to report and the fix loop in FixSpec has something to do.
same :: Int -> Int
same x = x
