module Presto.Core.LocalStorage where

import Prelude

import Data.Maybe (Maybe(..))

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

foreign import getValueFromLocalStoreImpl ::
  EffectFn1
    String
    String

foreign import setValueToLocalStoreImpl ::
  EffectFn2
    String
    String
    Unit

foreign import deleteValueFromLocalStoreImpl ::
  EffectFn1
    String
    Unit

getValueFromLocalStore :: String -> Aff (Maybe String)
getValueFromLocalStore k = let v = liftEffect $ runEffectFn1 getValueFromLocalStoreImpl k
                               in ifM ((==) "__failed" <$> v) (pure Nothing) (Just <$> v)

setValueToLocalStore :: String -> String -> Aff Unit
setValueToLocalStore k v = liftEffect $ runEffectFn2 setValueToLocalStoreImpl k v

deleteValueFromLocalStore :: String -> Aff Unit
deleteValueFromLocalStore k = liftEffect $ runEffectFn1 deleteValueFromLocalStoreImpl k