module Other where

-- The second package. An exemption written beside `pkg` must not
-- reach this module; one written at the root must.
twice :: Int -> Int
twice x = x + x
