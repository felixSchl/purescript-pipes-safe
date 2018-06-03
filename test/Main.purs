module Test.Main where

import Prelude

import Control.Monad.Aff (Milliseconds(Milliseconds), delay, error, killFiber, launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Debug.Trace (traceAnyA)
import Pipes (yield, (>->)) as P
import Pipes.Core (runEffect) as P
import Pipes.Prelude (drain) as P
import Pipes.Safe (register, runSafeP)

main :: forall e. Eff _ Unit
main = void $ launchAff do
  fiber <- liftEff $ launchAff $ P.runEffect $ runSafeP $
    (do
      P.yield 123
      liftAff $ delay $ 500.0 # Milliseconds
      void $ lift $ register \cond -> do
        traceAnyA cond
      liftAff $ delay $ 2000.0 # Milliseconds
    ) P.>-> P.drain
  -- joinFiber fiber
  liftAff $ delay $ 1000.0 # Milliseconds
  killFiber (error "test") fiber
