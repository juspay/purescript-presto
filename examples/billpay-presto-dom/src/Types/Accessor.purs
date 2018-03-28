module Storage.Accessor where

import Prelude ((>>>))
import Data.Lens (Lens', lens)
import Data.Newtype (class Newtype, unwrap, wrap)

_amount :: forall a b c. Newtype a {amount :: c | b} => Lens' a c
_amount = lens (unwrap >>> _.amount) (\oldRec newVal -> wrap ((unwrap oldRec) {amount = newVal}))

_mobileNumber :: forall a b c. Newtype a {mobileNumber :: c | b} => Lens' a c
_mobileNumber = lens (unwrap >>> _.mobileNumber) (\oldRec newVal -> wrap ((unwrap oldRec) {mobileNumber = newVal}))
 