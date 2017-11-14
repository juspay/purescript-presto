
module UI.Types where

import Control.Monad.Eff.Exception (Error)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Presto.Core.Flow (class Interact, defaultInteract)
import Product.Types (Operator,MobileNumber,Amount,BillPayStatus)

data InitScreen = InitScreen
data InitScreenAction = InitScreenRendered

data SplashScreen = SplashScreen
data SplashScreenAction = SplashScreenRendered

data ChooseOperatorScreen = ChooseOperatorScreen (Array Operator)
data ChooseOperatorScreenAction = OperatorSelected Operator | ChooseOperatorScreenAbort

data AskMobileNumberScreen = AskMobileNumberScreen
data AskMobileNumberScreenAction = SubmitMobileNumber MobileNumber | AskMobileNumberScreenAbort

data AskAmountScreen = AskAmountScreen
data AskAmountScreenAction = SubmitAmount Amount | AskAmountScreenAbort

data StatusScreen = StatusScreen MobileNumber Amount BillPayStatus
data StatusScreenAction = SuccessResult | StatusScreenAbort

-- Interact typeclass to tie input and output of the screen
instance splashScreenInteract :: Interact Error SplashScreen SplashScreenAction where
  interact x = defaultInteract x

-- To encode screen and state before sending to UI 
derive instance genericSplashScreen  :: Generic SplashScreen _
instance encodeSplashScreen :: Encode SplashScreen where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = false })

-- To decode action received from UI
derive instance genericSplashScreenAction  :: Generic SplashScreenAction _
instance decodegenericSplashScreenAction :: Decode SplashScreenAction where
  decode = genericDecode defaultOptions

instance initScreenInteract :: Interact Error InitScreen InitScreenAction where
  interact x = defaultInteract x

derive instance genericInitScreen  :: Generic InitScreen _
instance encodeInitScreen :: Encode InitScreen where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = false })

derive instance genericInitScreenAction  :: Generic InitScreenAction _
instance decodegenericInitScreenAction :: Decode InitScreenAction where
  decode = genericDecode defaultOptions

instance chooseOperatorScreenInteract :: Interact Error ChooseOperatorScreen ChooseOperatorScreenAction where
  interact x = defaultInteract x

derive instance genericChooseOperatorScreen  :: Generic ChooseOperatorScreen _
instance encodeChooseOperatorScreen :: Encode ChooseOperatorScreen where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = false })

derive instance genericChooseOperatorScreenAction  :: Generic ChooseOperatorScreenAction _
instance decodegenericChooseOperatorScreenAction :: Decode ChooseOperatorScreenAction where
  decode = genericDecode defaultOptions


instance askMobileNumberScreenInteract :: Interact Error AskMobileNumberScreen AskMobileNumberScreenAction where
  interact x = defaultInteract x

derive instance genericAskMobileNumberScreen  :: Generic AskMobileNumberScreen _
instance encodeAskMobileNumberScreen :: Encode AskMobileNumberScreen where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = false })

derive instance genericAskMobileNumberScreenAction  :: Generic AskMobileNumberScreenAction _
instance decodeGenericAskMobileNumberScreenAction :: Decode AskMobileNumberScreenAction where
  decode = genericDecode defaultOptions


instance askAmountScreenInteract :: Interact Error AskAmountScreen AskAmountScreenAction where
  interact x = defaultInteract x

derive instance genericAskAmountScreen  :: Generic AskAmountScreen _
instance encodeAskAmountScreen :: Encode AskAmountScreen where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = false })

derive instance genericAskAmountScreenAction  :: Generic AskAmountScreenAction _
instance decodegenericAskAmountScreenAction :: Decode AskAmountScreenAction where
  decode = genericDecode defaultOptions


instance statusScreenInteract :: Interact Error StatusScreen StatusScreenAction where
  interact x = defaultInteract x

derive instance genericStatusScreen  :: Generic StatusScreen _
instance encodeStatusScreen :: Encode StatusScreen where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = false })

derive instance genericStatusScreenAction  :: Generic StatusScreenAction _
instance decodegenericStatusScreenAction :: Decode StatusScreenAction where
  decode = genericDecode defaultOptions
