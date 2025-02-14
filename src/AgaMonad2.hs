{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AgaMonad2
  ( -- * Core Types
    StepCmd(..),
    AgaCmd(..),
    AgaMsg(..),
    TCMsg(..),
    AgdaCode(..),
    AgaTask(..),
    AgaState(..),
    AgaENV(..),
    AgaOutput(..),
    -- * The RWS stack for domain logic
    AgaMonad,
    -- * The Runtime Typeclass
    AgaRuntime(..),
    -- * Orchestration
    runAgaCmd,
    runAgaTask,
    -- * Lenses
    taskEnv,
    apiGptKey,
    tcUrl,
    llmModel,
    taskState,
    conversation,
    taskOutput
  ) where

import Control.Monad.Trans.RWS (RWST)
import Control.Monad.IO.Class      (liftIO)
import Control.Lens               (makeLenses, (^.), (%~))
import Control.Monad.State.Class   (MonadState, get, put, modify)
import Control.Monad.Reader.Class  (MonadReader, ask)
import Control.Monad.Writer.Class  (MonadWriter, tell, listen)
import Data.Text                   (Text)
import qualified Data.Text as T

----------------------------------------------------------------------------
-- 1. Basic Data Types
----------------------------------------------------------------------------

-- | Possible commands we can ask the runtime to perform.
data StepCmd
  = LLMReq Text       -- ^ Ask the LLM for something
  | TCReq AgdaCode    -- ^ Ask the type-checker
  | UserReq Text      -- ^ Ask the user
  deriving (Show)

-- | After we do a step, we either move to the next step or stop.
data AgaCmd
  = NextStep StepCmd
  | AgaStop
  deriving (Show)

-- | Replies from the runtime: LLM response, typechecker result, or user input.
data AgaMsg
  = LLMRes Text
  | TCRes TCMsg
  | UserRes Text
  deriving (Show)

-- | Typechecker result: success or error string
data TCMsg
  = TCSucces
  | TCErr Text
  deriving (Show)

-- | A chunk of Agda code, either a literal source or a file reference.
data AgdaCode
  = AgdaSource Text
  | AgdaFile FilePath
  deriving (Show)

-- | The domain-level environment.
data AgaENV eT = AgaENV
  { _taskEnv   :: eT     -- ^ environment for your specific task
  , _apiGptKey :: Text
  , _tcUrl     :: Text
  , _llmModel  :: Text
  -- ^ You can add a 'runtimeEnv' here if you want to store extra data
  }

-- | The domain-level state, storing your custom task state and conversation.
data AgaState sT = AgaState
  { _taskState    :: sT
  , _conversation :: [Message]
  }

-- | A message in the conversation, for example “user” or “assistant” roles.
data Message = Message
  { role    :: Text
  , content :: Text
  } deriving (Show)

-- | Output type that we accumulate in a writer, if needed.
data AgaOutput oT = AgaOutput
  { _taskOutput :: oT
  } deriving (Show)

-- | The domain-level “Task” definition.
data AgaTask aM eT oT sT mT =
  AgaTask
    { _firstStep ::  aM eT oT sT (mT, AgaCmd)
      -- ^ the first step, returning (model, next command)
    , _taskStep  :: (mT, AgaMsg) -> aM eT oT sT (mT, AgaCmd)
      -- ^ how to react to each new message
    , _initState :: sT
      -- ^ initial user-defined state
    }

-- | The RWS monad for domain logic (unchanged).
type AgaMonad eT oT sT =
  RWST
    (AgaENV eT)
    [AgaOutput oT]
    (AgaState sT)
    IO

----------------------------------------------------------------------------
-- Generate lenses
----------------------------------------------------------------------------
$(makeLenses ''AgaENV)
$(makeLenses ''AgaState)
$(makeLenses ''AgaOutput)
$(makeLenses ''AgaTask)

----------------------------------------------------------------------------
-- 2. 'AgaRuntime' Typeclass: The Runtime, Decoupled from 'AgaTask'
----------------------------------------------------------------------------

{-|
  A typeclass for "runtime" capabilities.

  This is decoupled from the 'AgaTask' or 'AgaMonad', so there's no functional
  dependency forcing 'm -> eT, sT, etc.'. The only assumption is that we can
  handle the three commands: LLMReq, TCReq, UserReq, returning an 'AgaMsg'.
-}
class (Monad m) => AgaRuntime m where
  handleLLMReq  :: Text -> m AgaMsg
  handleTCReq   :: AgdaCode -> m AgaMsg
  handleUserReq :: Text -> m AgaMsg

----------------------------------------------------------------------------
-- 3. A Helper to Run a Single 'StepCmd'
----------------------------------------------------------------------------

{-|
  'runAgaCmd' uses the 'AgaRuntime' typeclass to turn a 'StepCmd' into an 'AgaMsg'.
  This is a thin dispatcher so your domain logic doesn't need to know how
  LLMReq or TCReq is implemented.
-}
runAgaCmd :: (AgaRuntime m) => StepCmd -> m AgaMsg
runAgaCmd (LLMReq t)  = handleLLMReq t
runAgaCmd (TCReq c)   = handleTCReq c
runAgaCmd (UserReq u) = handleUserReq u

----------------------------------------------------------------------------
-- 4. 'runAgaTask': Orchestration of the Task's Loop
----------------------------------------------------------------------------

{-|
  A standalone function that:
    1. Sets the 'AgaState' to the task's init state.
    2. Runs the first step to get (model, next command).
    3. Loops until we get 'AgaStop'.

  Note that this does NOT require a 'FunctionalDependency' on the typeclass.
  We only ask that 'm' can:
    - handle the runtime calls (AgaRuntime m)
    - read the environment (MonadReader (AgaENV eT) m)
    - update the state (MonadState (AgaState sT) m)
    - produce outputs in the writer (MonadWriter [AgaOutput oT] m)

  This way, 'AgaTask' is totally decoupled from the runtime specifics.
-}
runAgaTask
  :: forall aM eT sT oT mT. 
    ( AgaRuntime (aM eT oT sT)
     -- , MonadReader (AgaENV eT) (aM eT oT sT) 
     , MonadState (AgaState sT) (aM eT oT sT)
     , MonadWriter [AgaOutput oT] (aM eT oT sT)
     )
  => AgaTask aM eT oT sT mT
  -> (aM eT oT sT) [oT]
runAgaTask agaTask = do
  -- 1. Set the user-defined initial state
  modify (\s -> s { _taskState = agaTask ^. initState })

  -- 2. Run the first step
  (mVal, agaCmd) <- agaTask ^. firstStep

  -- 3. Start the loop
  loop agaTask (mVal, agaCmd)

  -- 4. Collect the outputs from the writer
  (_, outs) <- listen (return ())
  return (map (^. taskOutput) outs)

 where
  -- loop :: aM eT oT sT mT
       -- -> (mT, AgaCmd)
       -- -> aM eT oT sT ()
  loop _  (_, AgaStop) = return ()
  loop at (mVal, NextStep stepCmd) = do
    -- Use the runtime class to handle the command
    agaMsg <- runAgaCmd stepCmd

    -- Pass the result back into the task's step function
    (mVal2, nextCmd) <- at ^. taskStep $ (mVal, agaMsg)

    loop at (mVal2, nextCmd)

----------------------------------------------------------------------------
-- 5. Example Instance for 'AgaMonad eT oT sT'
----------------------------------------------------------------------------

-- | A default or "real" runtime example. This is where you implement I/O calls.
instance AgaRuntime (AgaMonad eT oT sT) where
  handleLLMReq txt = do
    -- Example: read environment & conversation, call GPT or similar
    env <- ask
    st  <- get
    let conv = st ^. conversation
        key  = env ^. apiGptKey
        mdl  = env ^. llmModel
    -- For demonstration, just return a placeholder:
    return (LLMRes ("[Simulating GPT call with text: " <> txt <> "]"))

  handleTCReq agdaCode = do
    -- Example: read environment, call Agda checker
    env <- ask
    let url = env ^. tcUrl
    -- Again, placeholder:
    return (TCRes (TCSucces))

  handleUserReq prompt = do
    -- Example: read from user input or a UI channel
    return (UserRes "[User typed 'ok' - placeholder]")
  
