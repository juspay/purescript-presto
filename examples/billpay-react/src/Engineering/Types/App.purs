module Engineering.Types.App where


import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Prelude (pure, (<$>))
import Presto.Core.Types.Language.Flow (FlowWrapper)

type Flow e a = (ExceptT e (Free FlowWrapper) a)

liftLeft :: forall e b. e -> Flow e b
liftLeft e = ExceptT (Left <$> pure e)




