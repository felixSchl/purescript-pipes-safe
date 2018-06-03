module Pipes.Safe
  ( SafeT
  , runSafeT
  , runSafeP
  , register
  , ReleaseKey
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, class Plus)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Cont.Class (class MonadCont)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef', newRef)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Fork.Class (class MonadBracket, BracketCondition(..), bracket)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Data.Distributive (class Distributive)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse_)
import Data.Tuple (snd)
import Pipes.Core as P

newtype ReleaseKey = ReleaseKey Int

newtype Finalizers m = Finalizers
  { nextKey :: Int
  , finalizers :: M.Map Int (BracketCondition Error Unit -> m Unit)
  }

newtype SafeT m r =
  SafeT (ReaderT (Ref (Maybe (Finalizers m))) m r)

derive instance newtypeSafeT :: Newtype (SafeT m a) _
derive newtype instance functorSafeT :: Functor m => Functor (SafeT m)
derive newtype instance applySafeT :: Apply m => Apply (SafeT m)
derive newtype instance applicativeSafeT :: Applicative m => Applicative (SafeT m)
derive newtype instance altSafeT :: Alt m => Alt (SafeT m)
derive newtype instance plusSafeT :: Plus m => Plus (SafeT m)
derive newtype instance alternativeSafeT :: Alternative m => Alternative (SafeT m)
derive newtype instance bindSafeT :: Bind m => Bind (SafeT m)
derive newtype instance monadSafeT :: Monad m => Monad (SafeT m)
derive newtype instance monadZeroSafeT :: MonadZero m => MonadZero (SafeT m)
derive newtype instance monadAffSafeT :: MonadAff eff m => MonadAff eff (SafeT m)
derive newtype instance monadEffSafeT :: MonadEff eff m => MonadEff eff (SafeT m)
derive newtype instance semigroupSafeT :: (Apply m, Semigroup a) => Semigroup (SafeT m a)
derive newtype instance monoidSafeT :: (Applicative m, Monoid a) => Monoid (SafeT m a)
derive newtype instance monadPlusSafeT :: MonadPlus m => MonadPlus (SafeT m)
derive newtype instance monadTransSafeT :: MonadTrans (SafeT)
derive newtype instance monadContSafeT :: MonadCont m => MonadCont (SafeT m)
derive newtype instance monadThrowSafeT :: MonadThrow e m => MonadThrow e (SafeT m)
derive newtype instance monadErrorSafeT :: MonadError e m => MonadError e (SafeT m)
derive newtype instance monadStateSafeT :: MonadState s m => MonadState s (SafeT m)
derive newtype instance monadTellSafeT :: MonadTell w m => MonadTell w (SafeT m)
derive newtype instance monadWriterSafeT :: MonadWriter w m => MonadWriter w (SafeT m)
derive newtype instance distributiveSafeT :: Distributive m => Distributive (SafeT m)
-- derive newtype instance monadAskSafeT :: MonadAsk r m => MonadAsk r (SafeT m)
-- derive newtype instance monadReaderSafeT :: MonadReader r m => MonadReader r (SafeT m)

unSafeT :: ∀ m r. SafeT m r -> ReaderT (Ref (Maybe (Finalizers m))) m r
unSafeT (SafeT x) = x

runSafeT
  :: ∀ m eff fiber r
   . MonadEff (ref :: REF | eff) m
  => MonadBracket Error fiber m
  => SafeT m r
  -> m r
runSafeT m =
  bracket
    (liftEff $ newRef $ Just $ Finalizers { nextKey: 0, finalizers: mempty })
    (\cond ref -> do
      mres <-
        liftEff $
          modifyRef' ref \value ->
            { state: Nothing, value }
      case mres of
        Nothing ->
          pure unit
        Just (Finalizers { finalizers }) ->
          traverse_ (\x -> snd x $ voidBC cond) $
            M.toUnfoldable finalizers :: Array _
    )
    (runReaderT (unSafeT m))

voidBC :: ∀ e a. BracketCondition e a -> BracketCondition e Unit
voidBC (Completed _) = Completed unit
voidBC (Failed x) = Failed x
voidBC (Killed x) = Killed x

runSafeP
  :: ∀ m eff fiber r
   . MonadEff (ref :: REF | eff) m
  => MonadBracket Error fiber m
  => P.Effect (SafeT m) r
  -> P.Effect m r
runSafeP = lift <<< runSafeT <<< P.runEffect

register
  :: ∀ m eff fiber r
   . MonadEff (ref :: REF | eff) m
  => MonadAff (ref :: REF | eff) m
  => MonadBracket Error fiber m
  => (BracketCondition Error Unit -> m Unit)
  -> SafeT m ReleaseKey
register io = do
  ref <- SafeT ask
  mKey <- liftEff $
    modifyRef' ref \val ->
      case val of
        Nothing ->
          { state: Nothing, value: Nothing }
        Just (Finalizers x) ->
          { state:
              Just $
                Finalizers
                  { nextKey: x.nextKey + 1
                  , finalizers:
                      M.insert x.nextKey io x.finalizers
                  }
          , value: Just $ x.nextKey + 1
          }
  case mKey of
    Nothing ->
      liftAff $ throwError $ error "release: SafeT block is closed"
    Just k ->
      pure $ ReleaseKey k
