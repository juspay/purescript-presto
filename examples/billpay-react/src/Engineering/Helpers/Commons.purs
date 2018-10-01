module Engineering.Helpers.Commons where

import Prelude

import Effect (Effect)
import Effect.Exception (Error)
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2)
import Engineering.Types.App as App
import Presto.Core.Flow (runUI)
import Presto.Core.Types.API (Header(..), Headers(..), Request(..), URL)
import Presto.Core.Types.Language.Interaction (class Interact)

foreign import showUI' :: Fn2 (String -> Effect Unit) String (Effect Unit)
foreign import callAPI' :: AffError -> (AffSuccess String) -> NativeRequest -> (Effect Unit)

type NativeHeader = { field :: String , value :: String}
type NativeHeaders = Array NativeHeader
type AffError = (Error -> Effect Unit)
type AffSuccess s = (s -> Effect Unit)


newtype NativeRequest = NativeRequest
  { method :: String
  , url :: URL
  , payload :: String
  , headers :: NativeHeaders
  }


mkNativeRequest :: Request -> NativeRequest
mkNativeRequest (Request request@{headers: Headers hs}) = NativeRequest
                                          { method : show request.method
                                            , url: request.url
                                            , payload: request.payload
                                            , headers: mkNativeHeader <$> hs
                                            }

mkNativeHeader :: Header -> NativeHeader
mkNativeHeader (Header field val) = { field: field, value: val}


runUI' :: forall a b e. Interact Error a b => a -> App.Flow e b
runUI' a = ExceptT (Right <$> runUI a)

