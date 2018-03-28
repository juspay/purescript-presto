module Remote.Flow where

import Prelude (pure)

import Presto.Core.Flow (Flow)
import Types.Storage ( Transaction(Transaction), TransactionStatus(SUCCESS))
import Types.Remote (BillPayRequest(..))


fetchOperators :: Flow (Array String)
fetchOperators = pure operators

payBill :: BillPayRequest -> Flow Transaction
payBill (BillPayRequest req) = pure res
 where
  res = Transaction
    {
      mobileNumber: req.mobileNumber
     ,amount:req.amount
     ,operator:req.operator
     ,status:SUCCESS
    }

operators :: Array String
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