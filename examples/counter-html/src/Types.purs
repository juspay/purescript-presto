module Types where

import Prelude

import Effect.Aff (Aff)
import Presto.Core.Types.API (URL)
import Effect (Effect)
import Effect.Exception (Error)

type NativeHeader = { field :: String , value :: String}
type AffStorage = Aff
type EffStorage = Effect
type AffError = (Error -> Effect Unit)
type AffSuccess s = (s -> Effect Unit)
type NativeHeaders = Array NativeHeader

newtype NativeRequest = NativeRequest
 { method :: String
 , url :: URL
 , payload :: String
 , headers :: NativeHeaders
 }
