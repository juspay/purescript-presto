module Types.Storage where

import Data.Newtype

newtype Transaction = Transaction 
    {
      mobileNumber::String
     ,amount::Number
     ,operator::String
     ,status :: TransactionStatus
    }

derive instance newtypeTransaction :: Newtype Transaction _

data TransactionStatus = SUCCESS | FAILURE | PENDING