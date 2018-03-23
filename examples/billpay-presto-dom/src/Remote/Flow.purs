module Remote.Flow where

import Prelude

import Presto.Core.Flow (Flow)
import Types.UI (BillPayStatus(..), MobileNumber, Operator, Amount)

operators :: Array Operator
operators = [
    "Airtel"
  , "Vodafone"
  , "JIO"
  , "Aircel"
  , "Docomo"
  , "BSNL"
  , "Idea"
  , "MTNL"
  , "Airtel"
  , "Vodafone"
  , "JIO"
  , "Aircel"
  , "Docomo"
  , "BSNL"
  , "Idea"
  , "MTNL"
]

fetchOperators :: Flow (Array Operator)
fetchOperators = pure operators

payBill :: MobileNumber -> Amount -> Operator -> Flow BillPayStatus
payBill mobileNumber amount operator= pure SUCCESS