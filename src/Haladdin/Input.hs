module Haladdin.Input where

import qualified Data.Set as Set
import           Data.Set (Set)
import           Haladdin.Model
import           Miso

-- | The keyboard keys we will use
keyMapping :: GKeysState Int
keyMapping = KeysState 37 39 -- left, right
                       40 38 -- down, up
                       32 -- jump = space
                       18 -- throw = alt/meta
                       17 -- slash = ctrl

-- | Subscribe to keystate changes
keysStateSub   :: (KeysState -> action) -> Sub action
keysStateSub f = keyboardSub (\ks -> mkAction f ks)


mkAction     :: (KeysState -> action) -> Set Int -> action
mkAction f s = f $ fmap check keyMapping
  where
    check i | i `Set.member` s = Pressed
            | otherwise        = Released
