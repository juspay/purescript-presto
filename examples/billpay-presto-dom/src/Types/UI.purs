module Types.UI where

type MobileNumber = String
type Amount = Number
type Operator = String

data BillPayStatus = SUCCESS | FAILURE
data BillPayFailure = FetchOperatorFailure String 
					           | BillPaymentFailure String 
					           | UserAbort
