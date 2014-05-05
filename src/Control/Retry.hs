{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Retry
-- Copyright   :  Ozgun Ataman <ozgun.ataman@soostone.com>
-- License     :  BSD3
--
-- Maintainer  :  Ozgun Ataman
-- Stability   :  provisional
--
-- This module exposes combinators that can wrap arbitrary monadic
-- actions. They run the action and potentially retry running it with
-- some configurable delay for a configurable number of times.
--
-- The express purpose of this library is to make it easier to work
-- with IO and especially network IO actions that often experience
-- temporary failure that warrant retrying of the original action. For
-- example, a database query may time out for a while, in which case
-- we should delay a bit and retry the query.
----------------------------------------------------------------------------


module Control.Retry
    (
      -- * High Level Operation
      RetrySettings (..)
    , RetryLimit(..)
    , limitedRetries
    , unlimitedRetries

    , retrying
    , recovering
    , recoveringWatchdog
    , recoverAll

    -- * Utilities
    , delay
    , performDelay
    , flatDelay
    , backoffDelay
    , backoffDelayFor
    ) where

-------------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async.Lifted
import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Base
import           Control.Monad.Trans.Control
import           Control.Monad.IO.Class
import           Control.Concurrent.MVar.Lifted
import           Data.Default.Class
-------------------------------------------------------------------------------


data RetryLimit = RLimit Int
                | RNoLimit


-- | Set a limited number of retries. Default in 'def' is 5.
limitedRetries :: Int -> RetryLimit
limitedRetries = RLimit


-- | Set an unlimited number of retries. Note that with this option
-- turned on, the combinator will keep retrying the action
-- indefinitely and might essentially hang in some cases.
unlimitedRetries :: RetryLimit
unlimitedRetries = RNoLimit


-- | Settings for retry behavior. Simply using 'def' for default
-- values should work in most cases.
data RetrySettings = RetrySettings {
      numRetries :: RetryLimit
    -- ^ Number of retries. Defaults to 5.
    , backoff    :: Bool
    -- ^ Whether to implement exponential backoff in retries. Defaults
    -- to True.
    , baseDelay  :: Int
    -- ^ The base delay in miliseconds. Defaults to 50. Without
    -- 'backoff', this is the delay. With 'backoff', this base delay
    -- will grow by a factor of 2 on each subsequent retry.
    }


instance Default RetrySettings where
    def = RetrySettings (limitedRetries 5) True 50


-- | Delay thread using backoff delay for the nth retry.
backoffDelay :: MonadIO m => RetrySettings -> Int -> m ()
backoffDelay set !n = liftIO . threadDelay $ backoffDelayFor (delay set) n


-- | Delay for nth iteration of exponential backoff, in microseconds
backoffDelayFor
    :: Int
    -- ^ Base delay in microseconds
    -> Int
    -- ^ Iteration number, starting at 0.
    -> Int
backoffDelayFor base n = 2^n * base


-- | Delay thread using flat delay
flatDelay :: MonadIO m => RetrySettings -> t -> m ()
flatDelay set@RetrySettings{..} !_ = liftIO (threadDelay $ delay set)

-- | Delay in micro seconds
delay :: RetrySettings -> Int
delay RetrySettings{..} = baseDelay * 1000


-- | Retry combinator for actions that don't raise exceptions, but
-- signal in their type the outcome has failed. Examples are the
-- 'Maybe', 'Either' and 'EitherT' monads.
--
-- Let's write a function that always fails and watch this combinator
-- retry it 5 additional times following the initial run:
--
-- >>> import Data.Maybe
-- >>> let f = putStrLn "Running action" >> return Nothing
-- >>> retrying def isNothing f
-- Running action
-- Running action
-- Running action
-- Running action
-- Running action
-- Running action
-- Nothing
--
-- Note how the latest failing result is returned after all retries
-- have been exhausted.
retrying :: MonadIO m
         => RetrySettings
         -> (b -> Bool)
         -- ^ A function to check whether the result should be
         -- retried. If True, we delay and retry the operation.
         -> m b
         -- ^ Action to run
         -> m b
retrying set@RetrySettings{..} chk f = go 0
    where
      retry n = do
          performDelay set n
          go $! n+1

      go n = do
          res <- f
          case chk res of
            True ->
              case numRetries of
                RNoLimit -> retry n
                RLimit lim -> if n >= lim then return res else retry n
            False -> return res



-- | Retry ALL exceptions that may be raised. To be used with caution;
-- this matches the exception on 'SomeException'.
--
-- See how the action below is run once and retried 5 more times
-- before finally failing for good:
--
-- >>> let f = putStrLn "Running action" >> error "this is an error"
-- >>> recoverAll def f
-- Running action
-- Running action
-- Running action
-- Running action
-- Running action
-- Running action
-- *** Exception: this is an error
recoverAll :: (MonadIO m, MonadCatch m)
         => RetrySettings
         -> m a
         -> m a
recoverAll set f = recovering set [h] f
    where
      h = Handler $ \ (_ :: SomeException) -> return True


-- | Perform 'threadDelay' for the nth retry for the given settings.
performDelay :: MonadIO m => RetrySettings -> Int -> m ()
performDelay set@RetrySettings{..} n =
    if backoff
      then backoffDelay set n
      else flatDelay set n


-- | Run an action and recover from a raised exception by potentially
-- retrying the action a number of times.
recovering :: forall m a. (MonadIO m, MonadCatch m)
           => RetrySettings
           -- ^ Just use 'def' faor default settings
           -> [Handler m Bool]
           -- ^ Should a given exception be retried? Action will be
           -- retried if this returns True.
           -> m a
           -- ^ Action to perform
           -> m a
recovering set@RetrySettings{..} hs f = go 0
    where
      retry n = do
          performDelay set n
          go $! n+1


      -- | Convert a (e -> m Bool) handler into (e -> m a) so it can
      -- be wired into the 'catches' combinator.
      transHandler :: Int -> Handler m Bool -> Handler m a
      transHandler n (Handler h) = Handler $ \ e -> do
          chk <- h e
          case chk of
            True ->
              case numRetries of
                RNoLimit -> retry n
                RLimit lim -> if n >= lim then throwM e else retry n
            False -> throwM e

      go n = f `catches` map (transHandler n) hs

-- | Run a long running action and recover from a raised exception
-- by potentially retrying the action a number of times.
-- If the action keeps running for longer than the provided reset delay,
-- the retry count is reset.
--
-- /Note 1:/ I only got good results with reset delays >= 2 ms.
--
-- /Note 2:/ This is appropriate for unstable servers: it allows you to restart them
-- indefinitely, but exit if they keep failing after the configured number
-- of retries with very short executions.
--
-- /Note 3:/ actually, delayThreadAsync may take longer than expected,
-- and thus the value is not reset... how to cope with this?
-- We don't actually have real time garantees in Haskell, so a failed
-- execution actually passes by as successful. Is this ok?
-- It has to be...


recoveringWatchdog
  :: forall m a.  ( MonadIO m
                  , MonadCatch m
                  , MonadBase IO m
                  , MonadBaseControl IO m)
  => RetrySettings
  -- ^ Just use 'def' faor default settings
  -> Int
  -- ^ Reset delay (in ms). If the process keeps running after this delay,
  -- reset the retry count to zero.
  -> [Handler m Bool]
  -- ^ Should a given exception be retried? Action will be
  -- retried if this returns True.
  -> m a
  -- ^ Action to perform
  -> m a
recoveringWatchdog set@RetrySettings{..} rstDelay hs f = do
  retryCount <- newMVar 0
  go retryCount

    where
      retry :: MVar Int -> m a
      retry retryCount = do
          n <- readMVar retryCount
          performDelay set n
          modifyMVar_ retryCount (\i -> return $! i + 1)
          go $! retryCount


      -- | Convert a (e -> m Bool) handler into (e -> m a) so it can
      -- be wired into the 'catches' combinator.
      transHandler :: MVar Int -> Handler m Bool -> Handler m a
      transHandler retryCount (Handler h) = Handler $ \ e -> do
          chk <- h e
          n   <- liftIO $ readMVar retryCount
          if chk
            then case numRetries of
                RNoLimit   -> retry retryCount
                RLimit lim -> if n >= lim
                                then throwM e
                                else retry retryCount
            else throwM e

      go :: MVar Int -> m a
      go retryCount = do
        delayThreadAsync <- async (liftIO . threadDelay $ rstDelay * 1000)
        actionAsync      <- async f

        which <- waitEither delayThreadAsync actionAsync
                `catches` map (fmap Right <$> transHandler retryCount) hs
        case which of
          -- time out finished, main action is still executing:
          -- reset counter
          Left _ -> do
            modifyMVar_ retryCount (const $! return $! 0)
            wait actionAsync `catches` map (transHandler retryCount) hs
          -- otherwise just return the result
          Right ok ->  do
            cancel delayThreadAsync -- useless
            return ok



                              ------------------
                              -- Simple Tests --
                              ------------------



-- data TestException = TestException deriving (Show, Typeable)
-- data AnotherException = AnotherException deriving (Show, Typeable)

-- instance Exception TestException
-- instance Exception AnotherException


-- test = retrying def [h1,h2] f
--     where
--       f = putStrLn "Running action" >> throw AnotherException
--       h1 = Handler $ \ (e :: TestException) -> return False
--       h2 = Handler $ \ (e :: AnotherException) -> return True
