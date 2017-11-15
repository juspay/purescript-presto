module Product.Types where

import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Generic.Rep (class Generic)

type MobileNumber = String
type Amount = Number
type Operator = String

data BillPayStatus = SUCCESS | FAILURE
data BillPayFailure = FetchOperatorFailure String | BillPaymentFailure String | UserAbort

derive instance genericBillPayStatus  :: Generic BillPayStatus _
instance encodeBillPayStatus :: Encode BillPayStatus where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = false })