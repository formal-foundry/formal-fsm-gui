{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AgaMock
  ( -- * The Mock Runtime
    AgdaMock(..),
    runAgdaMock
  ) where

import AgaMonad2
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Control.Monad.Trans.RWS  (RWST, runRWST)
import Control.Monad.Reader     (MonadReader)
import Control.Monad.State      (MonadState)
import Control.Monad.Writer     (MonadWriter)
import System.Random            (randomIO, randomRIO)
import Data.Text                (Text)
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- | A newtype wrapper around the same RWST as 'AgaMonad', but specialized
--   for mocking. This makes it easy to define a separate 'AgaRuntime' instance.
-------------------------------------------------------------------------------
newtype AgdaMock eT oT sT a =
  AgdaMock { unAgdaMock :: RWST (AgaENV eT) [AgaOutput oT] (AgaState sT) IO a }
  deriving (Functor, Applicative, Monad, MonadIO,
            MonadReader (AgaENV eT),
            MonadState (AgaState sT),
            MonadWriter [AgaOutput oT])

-------------------------------------------------------------------------------
-- | Run the 'AgdaMock' action, returning result, final state, and any outputs.
-------------------------------------------------------------------------------
runAgdaMock 
  :: AgdaMock eT oT sT a            -- ^ The action to run
  -> AgaENV eT                      -- ^ The environment
  -> AgaState sT                    -- ^ The initial state
  -> IO (a, AgaState sT, [AgaOutput oT])
runAgdaMock action env st = runRWST (unAgdaMock action) env st

-------------------------------------------------------------------------------
-- | The Mock instance: Randomly simulate LLM, Typechecker, and User responses.
--   Also print progress to stdout as requested.
-------------------------------------------------------------------------------
instance AgaRuntime (AgdaMock eT oT sT) where

  -- LLM mock: prints a message and returns a random "LLMRes".
  handleLLMReq txt = do
    liftIO $ putStrLn ("[Mock LLM Request] " <> T.unpack txt)
    r :: Int <- liftIO $ randomRIO (1,3)
    let response = case r of
          1 -> "Mock LLM: success path"
          2 -> "Mock LLM: alternative path"
          _ -> "Mock LLM: random variation"
    liftIO $ putStrLn (" -> Mock LLM responding with: " <> T.unpack response)
    return (LLMRes response)

  -- Typechecker mock: 50% success, 50% error, prints to stdout.
  handleTCReq agdaCode = do
    liftIO $ putStrLn ("[Mock Typechecker Request] " <> show agdaCode)
    r :: Bool <- liftIO randomIO
    if r
       then do
         liftIO $ putStrLn " -> Typechecker result: TCSucces"
         return (TCRes TCSucces)
       else do
         liftIO $ putStrLn " -> Typechecker result: TCErr"
         return (TCRes (TCErr "Random mock type error"))

  -- User mock: picks a random response from a small set.
  handleUserReq prompt = do
    liftIO $ putStrLn ("[Mock User Prompt] " <> T.unpack prompt)
    r :: Int <- liftIO $ randomRIO (1,3)
    let response = case r of
          1 -> "Yes, sure!"
          2 -> "No, never."
          _ -> "Hmm, maybe?"
    liftIO $ putStrLn (" -> Mock user typed: " <> T.unpack response)
    return (UserRes response)
