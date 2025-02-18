{-# LANGUAGE OverloadedStrings #-}

module LLMParserTest where

import Data.Text (Text)
import qualified Data.Text as T
import LLMParser
  ( parseLLMResponse
  , TrLLMResult(..)
  , ResponseError(..)
  )

--------------------------------------------------------------------------------
-- 1. A data type to represent what we *expect* from a given test case
--------------------------------------------------------------------------------

data ExpectedResult
  = ExpectClarifications Text
  | ExpectAgdaSnippet Text
  | ExpectError ResponseError
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 2. A structured test case: label + input + expected outcome
--------------------------------------------------------------------------------

data TestCase = TestCase
  { tcLabel    :: String
  , tcInput    :: Text
  , tcExpected :: ExpectedResult
  }

--------------------------------------------------------------------------------
-- 3. The core function that runs a single test and returns pass/fail + details
--------------------------------------------------------------------------------

data TestResult = TestPassed | TestFailed String
  deriving (Show, Eq)

runTestCase :: TestCase -> IO TestResult
runTestCase (TestCase label input expected) = do
  let actual = parseLLMResponse input
      outcome = checkOutcome actual expected
  case outcome of
    Just err -> return (TestFailed (label ++ ": " ++ err))
    Nothing  -> return TestPassed

-- | Compare the parser's actual result (or error) with the expected result.
checkOutcome
  :: Either ResponseError TrLLMResult
  -> ExpectedResult
  -> Maybe String
checkOutcome (Right (TrLLMClarifications clar)) (ExpectClarifications txtExp)
  | clar == txtExp = Nothing
  | otherwise      = Just $
      "Clarifications mismatch.\n  Expected: " ++ show txtExp
      ++ "\n  Actual:   " ++ show clar

checkOutcome (Right (TrLLMAgdaSnippet snippet)) (ExpectAgdaSnippet txtExp)
  | snippet == txtExp = Nothing
  | otherwise         = Just $
      "Snippet mismatch.\n  Expected: " ++ show txtExp
      ++ "\n  Actual:   " ++ show snippet

checkOutcome (Left err) (ExpectError eExp)
  | err == eExp = Nothing
  | otherwise   = Just $
      "Error mismatch.\n  Expected: " ++ show eExp
      ++ "\n  Actual:   " ++ show err

-- If we got clarifications but expected something else
checkOutcome (Right (TrLLMClarifications clar)) _ =
  Just $
    "Got clarifications, but expected a snippet or error.\n  Actual: " ++ show clar

-- If we got a snippet but expected something else
checkOutcome (Right (TrLLMAgdaSnippet snippet)) _ =
  Just $
    "Got an Agda snippet, but expected clarifications or error.\n  Actual: " ++ show snippet

-- If we got an error but expected a different success
checkOutcome (Left err) _ =
  Just $
    "Got an error, but expected success.\n  Actual: " ++ show err

--------------------------------------------------------------------------------
-- 4. The list of test cases
--------------------------------------------------------------------------------

testCases :: [TestCase]
testCases =
  [ TestCase
      { tcLabel = "Test1 - No code block"
      , tcInput = "I'm just clarifying some details, no code block here!"
      , tcExpected = ExpectClarifications
          "I'm just clarifying some details, no code block here!"
      }

  , TestCase
      { tcLabel = "Test2 - Single valid code block"
      , tcInput = T.unlines
          [ "Here is my Agda snippet:"
          , "```agda"
          , "postulate MyType : Set"
          , "```"
          , "That's all!"
          ]
      -- We note the parser includes a trailing newline in snippet,
      -- because T.breakOn returns everything up to next fence, including "\n".
      -- Easiest fix: just match it exactly.
      , tcExpected = ExpectAgdaSnippet "postulate MyType : Set\n"
      }

  , TestCase
      { tcLabel = "Test3 - Multiple code blocks"
      , tcInput = T.unlines
          [ "First block:"
          , "```agda"
          , "postulate MyType1 : Set"
          , "```"
          , "Second block:"
          , "```agda"
          , "postulate MyType2 : Set"
          , "```"
          ]
      , tcExpected = ExpectError MultipleCodeBlocks
      }

  , TestCase
      { tcLabel = "Test4 - Partial fence, mismatch"
      , tcInput = "```agd\npostulate Broken : Set\n```"
      , tcExpected = ExpectClarifications "```agd\npostulate Broken : Set\n```"
      }

  , TestCase
      { tcLabel = "Test5 - Fenced code without 'agda' tag"
      , tcInput = T.unlines
          [ "```"
          , "postulate AnotherBroken : Set"
          , "```"
          ]
      , tcExpected = ExpectClarifications $ T.unlines
          [ "```"
          , "postulate AnotherBroken : Set"
          , "```"
          ]
      }

  , TestCase
      { tcLabel = "Test6 - Single multiline snippet"
      , tcInput = T.unlines
          [ "Let me try a multiline snippet..."
          , "```agda"
          , "module MyModule where"
          , ""
          , "data MyNat : Set where"
          , "  zero : MyNat"
          , "  suc  : MyNat -> MyNat"
          , ""
          , "postulate something : MyNat -> Set"
          , "```"
          , "That’s it!"
          ]
      -- We'll also expect that trailing newline in the snippet:
      , tcExpected = ExpectAgdaSnippet $ T.unlines
          [ "module MyModule where"
          , ""
          , "data MyNat : Set where"
          , "  zero : MyNat"
          , "  suc  : MyNat -> MyNat"
          , ""
          , "postulate something : MyNat -> Set"
          ]
      }

  , TestCase
      { tcLabel = "Test7 - Empty input"
      , tcInput = ""
      , tcExpected = ExpectClarifications ""
      }

  , TestCase
      { tcLabel = "Test8 - Whitespace only"
      , tcInput = "    \n  "
      , tcExpected = ExpectClarifications "    \n  "
      }

  , TestCase
      { tcLabel = "Test9 - Missing closing fence"
      , tcInput = T.unlines
          [ "```agda"
          , "data Orphan : Set"
          , "-- missing the closing fence (but no triple backticks!)"
          ]
      , tcExpected = ExpectError MalformedCodeBlock
      }

  , TestCase
      { tcLabel = "Test10 - Extra backticks inside snippet"
      , tcInput = T.unlines
          [ "```agda"
          , "postulate Strange : Set"
          , "  -- here's some random backtick: `` but not triple"
          , "```"
          ]
      , tcExpected = ExpectAgdaSnippet $ T.unlines
          [ "postulate Strange : Set"
          , "  -- here's some random backtick: `` but not triple"
          ]
      }
  ]

--------------------------------------------------------------------------------
-- 5. Main: run all tests, show pass/fail, summarize
--------------------------------------------------------------------------------
swap (a,b) = (b,a)

main :: IO ()
main = do
  putStrLn "Running LLMParserTest...\n"
  results <- mapM runTestCase testCases

  let labeledResults = zip results (map tcLabel testCases)
      passCount = length [ () | (TestPassed, _) <- labeledResults ]
      failCount = length [ () | (TestFailed _, _) <- labeledResults ]
      totalTests = length labeledResults

  -- Print per-test outcomes
  mapM_ (printResult .  swap) $ zip testCases results

  -- Print summary
  putStrLn "========================================"
  putStrLn $ "Total tests run: " ++ show totalTests
  putStrLn $ "Passed: " ++ show passCount
  putStrLn $ "Failed: " ++ show failCount

printResult :: (TestResult, TestCase) -> IO ()
printResult (TestPassed, tc) = do
  putStrLn $ tcLabel tc ++ ": PASSED"
printResult (TestFailed errMsg, tc) = do
  putStrLn $ tcLabel tc ++ ": FAILED"
  putStrLn $ "  Reason: " ++ errMsg
  putStrLn ""
