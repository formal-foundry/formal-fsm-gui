{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import           AgaMonad2
import           Control.Lens          ((^.))
import           Control.Monad.RWS     (runRWST)
import           Control.Monad.Trans.RWS (RWST)
import           Control.Monad.State   (modify)
import           Control.Monad.Writer  (tell)
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           GHC.Generics          (Generic)
import           System.Environment    (getArgs)
import           System.Directory      (doesFileExist)
import qualified Data.Yaml as Y

--------------------------------------------------------------------------------
-- 1. Configuration Data
--------------------------------------------------------------------------------

-- | We'll define a simple Config type that corresponds to the fields we need.
data Config = Config
  { cfgApiKey :: Text
  , cfgTcUrl  :: Text
  , cfgModel  :: Text
  } deriving (Show, Generic)

-- Make it an instance of FromJSON for easy YAML decoding.
instance Y.FromJSON Config

-- | Default fallback config, in case no file is provided
--   and we don't get certain flags from cmdline.
defaultConfig :: Config
defaultConfig = Config
  { cfgApiKey = "YOUR_DEFAULT_API_KEY"
  , cfgTcUrl  = "http://localhost:8080"
  , cfgModel  = "gpt-3.5-turbo"
  }

--------------------------------------------------------------------------------
-- 2. Loading the Config from File and/or Command Line
--------------------------------------------------------------------------------

-- | Load config from a YAML file, if it exists and is valid.
loadConfigFile :: FilePath -> IO (Either String Config)
loadConfigFile filePath = do
  exists <- doesFileExist filePath
  if not exists
    then return (Left $ "Config file not found: " ++ filePath)
    else do
      eVal <- Y.decodeFileEither filePath
      case eVal of
        Left err   -> return (Left $ "YAML parse error: " ++ show err)
        Right conf -> return (Right conf)

-- | Parse command-line arguments in a simplistic way:
--   --config <path> --key <apiKey> --url <tcUrl> --model <modelName>
parseArgs :: [String] -> IO (Maybe FilePath, Maybe Text, Maybe Text, Maybe Text)
parseArgs [] = return (Nothing, Nothing, Nothing, Nothing)
parseArgs ("--config":path:xs) = do
  (mC, mK, mU, mM) <- parseArgs xs
  return (Just path, mK, mU, mM)
parseArgs ("--key":k:xs) = do
  (mC, mK, mU, mM) <- parseArgs xs
  return (mC, Just (T.pack k), mU, mM)
parseArgs ("--url":u:xs) = do
  (mC, mK, mU, mM) <- parseArgs xs
  return (mC, mK, Just (T.pack u), mM)
parseArgs ("--model":m:xs) = do
  (mC, mK, mU, mM) <- parseArgs xs
  return (mC, mK, mU, Just (T.pack m))
parseArgs (_:xs) = parseArgs xs

--------------------------------------------------------------------------------
-- 3. Example Domain-Specific Environment, State, Output
--------------------------------------------------------------------------------

-- | For demonstration, let's define a custom environment extension.
--   If you already have an `eT` type for your domain environment, use that instead.
data MyEnvData = MyEnvData
  { someEnvField :: Int
    -- ^ Put any domain-specific environment fields here
  } deriving (Show)

-- | Similarly, define your domain-specific state.
data MyStateData = MyStateData
  { counter :: Int
    -- ^ Example counter, or any custom state data
  } deriving (Show)

-- | The output we accumulate in the Writer. Adjust as needed.
data MyOutputData = MyOutputData
  { resultSummary :: Text
  } deriving (Show)

--------------------------------------------------------------------------------
-- 4. Constructing the AgaENV and AgaState
--------------------------------------------------------------------------------

-- | Build the runtime `AgaENV eT` using the loaded config and your own domain data.
mkAgaEnv :: Config -> AgaENV MyEnvData
mkAgaEnv cfg = AgaENV
  { _taskEnv   = MyEnvData { someEnvField = 42 }
  , _apiGptKey = cfgApiKey cfg
  , _tcUrl     = cfgTcUrl cfg
  , _llmModel  = cfgModel cfg
  }

-- | Initial domain state. In a real app, load or compute it as needed.
initMyState :: MyStateData
initMyState = MyStateData
  { counter = 0
  }

-- | Start with an empty conversation or pre-populate if needed.
initAgaState :: MyStateData -> AgaState MyStateData
initAgaState s =
  AgaState
    { _taskState    = s
    , _conversation = []  -- no prior messages
    }

--------------------------------------------------------------------------------
-- 5. Define a Sample Task
--------------------------------------------------------------------------------

-- | We'll define a model type used internally in the task's steps.
data MyModel = MyModel
  { someField :: Text
  } deriving (Show)

-- | A trivial example task to show how you might tie it all together.
myTask :: AgaTask (AgaMonad MyEnvData MyOutputData MyStateData) 
                 MyEnvData  -- eT
                 MyOutputData -- oT
                 MyStateData  -- sT
                 MyModel      -- mT
myTask = AgaTask
  { _firstStep = do
      -- Possibly do some initialization logic
      let modelVal = MyModel "Initial model content"
      -- Output something to the writer
      tell [AgaOutput (MyOutputData "First step started.")]
      -- Next, specify the next command to the runtime (we'll do an LLM request).
      return (modelVal, NextStep (LLMReq "Hello, LLM!"))

  , _taskStep = \(modelVal, agaMsg) -> do
      case agaMsg of
        LLMRes txt -> do
          -- We got a response from the LLM
          tell [AgaOutput (MyOutputData ("LLM said: " <> txt))]
          -- Update the state or do domain logic
          modify (\st -> st { _taskState = (st ^. taskState) { counter = counter (st ^. taskState) + 1 } })
          -- Perhaps ask for user input next
          return (modelVal { someField = "Updated from LLMRes" }, NextStep (UserReq "Please provide input."))

        UserRes userTxt -> do
          tell [AgaOutput (MyOutputData ("User typed: " <> userTxt))]
          -- Maybe we do another step or we can stop
          -- Let's do a type-check next
          return (modelVal, NextStep (TCReq (AgdaSource "module Demo where\npostulate A : Set")))

        TCRes tcMsg -> do
          case tcMsg of
            TCSucces     -> tell [AgaOutput (MyOutputData "Agda check succeeded.")]
            TCErr errTxt -> tell [AgaOutput (MyOutputData ("Agda check error: " <> errTxt))]
          -- Now let's stop
          return (modelVal, AgaStop)
  , _initState = initMyState
  }

--------------------------------------------------------------------------------
-- 6. Orchestrate it all in `main`
--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- 6.1. Parse command-line arguments
  args <- getArgs
  (mConfigFile, mApiKey, mTcUrl, mModel) <- parseArgs args

  -- 6.2. Load config from file (if requested) or use default
  baseConf <- case mConfigFile of
                Just fp -> do
                  eLoaded <- loadConfigFile fp
                  case eLoaded of
                    Left err  -> do
                      putStrLn $ "Warning: " ++ err
                      return defaultConfig
                    Right c   -> return c
                Nothing -> return defaultConfig

  -- 6.3. Merge config with any overriding command-line flags
  let finalConf = baseConf
        { cfgApiKey = fromMaybe (cfgApiKey baseConf) mApiKey
        , cfgTcUrl  = fromMaybe (cfgTcUrl  baseConf) mTcUrl
        , cfgModel  = fromMaybe (cfgModel  baseConf) mModel
        }

  -- 6.4. Create the environment and initial AgaState
  let env     = mkAgaEnv finalConf
  let agaSt   = initAgaState initMyState

  -- 6.5. Run the AgaTask in the AgaMonad
  putStrLn "Running AgaTask..."
  (res, finalState, writerLog) <- runRWST (runAgaTask myTask) env agaSt

  -- 6.6. Handle the results
  putStrLn "Task completed."
  putStrLn "======================="
  putStrLn "Collected outputs from the writer (the `[oT]` from runAgaTask itself):"
  mapM_ print res

  putStrLn "\nComplete log stored by the Writer (the `[AgaOutput oT]`):"
  mapM_ print writerLog

  putStrLn "\nFinal state (including conversation, etc.):"
  print finalState

  putStrLn "\nDone."
