{-| Validators for testing
-}
module Armadillo.Test.Validator(
  alwaysSucceedsValidator
  ) where

import           PlutusTx.Prelude (BuiltinData)

{-# INLINABLE alwaysSucceedsValidator #-}
alwaysSucceedsValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysSucceedsValidator _ _ _ = ()
