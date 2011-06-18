module Soulgem (
                apply0
) where
import LTG


-- Love me do
-- v[field] <- v[field] v[0]
-- S (K v[field]) Get Zero = v[field] (Get Zero)
-- Construct cost: 4
-- Execution cost: -
apply0 :: Int -> LTG ()
apply0 field = do
  K $> field
  S $> field
  field $< Get
  field $< Zero
