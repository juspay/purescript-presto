module Presto.Core.LocalStorage where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Presto.Core.Types.App (LOCAL_STORAGE)

foreign import getValueFromLocalStore' :: forall e. String -> Eff (ls :: LOCAL_STORAGE | e) String
foreign import setValueToLocalStore' :: forall e. String -> String -> Eff (ls :: LOCAL_STORAGE | e) Unit

getValueFromLocalStore :: forall eff. String -> Aff (ls :: LOCAL_STORAGE | eff) (Maybe String)
getValueFromLocalStore k = let v = liftEff $ getValueFromLocalStore' k
                               in ifM ((==) "__failed" <$> v) (pure Nothing) (Just <$> v)

setValueToLocalStore :: forall eff. String -> String -> Aff (ls :: LOCAL_STORAGE | eff) Unit
setValueToLocalStore k v = liftEff $ setValueToLocalStore' k v
