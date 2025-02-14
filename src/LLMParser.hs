{-# LANGUAGE OverloadedStrings #-}

module LLMParser
  ( TrLLMResult(..)
  , ResponseError(..)
  , parseLLMResponse
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Possible outcomes from the LLM.
data TrLLMResult
  = TrLLMClarifications Text   -- ^ LLM provided clarifying questions/statements (no code block).
  | TrLLMAgdaSnippet Text      -- ^ LLM provided a single Agda code snippet.
  deriving (Show) 

-- | Possible errors when parsing the LLM response.
data ResponseError
  = MultipleCodeBlocks
  | NoRecognizedContent
  | MalformedCodeBlock
  deriving (Show, Eq)

-- | A parser that does a simple manual search for exactly one code block
--   fenced by:
--
--       ```agda
--       ... <snippet> ...
--       ```
--
--   If no such code block is found, we assume clarifications only.
--   If more than one such block is detected, or if the block is incomplete,
--   we return an error.
parseLLMResponse :: Text -> Either ResponseError TrLLMResult
parseLLMResponse input =
  let -- 1) Try to find the first occurrence of ```agda
      (before, afterStart) = T.breakOn "```agda" input
  in if T.null afterStart
       ------------------------------------------------------------------------
       -- 1a) No "```agda" found => treat as clarifications.
       ------------------------------------------------------------------------
       then Right (TrLLMClarifications input)
       else
         -- We found a "```agda" fence. Drop that fence from the text:
         let afterTag = T.drop (T.length "```agda") afterStart
             -- Optionally skip whitespace/newlines right after ```agda
             afterWhite = T.dropWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r') afterTag

             -- 2) Find the ending fence "```"
             (snippet, afterSnippet) = T.breakOn "```" afterWhite
         in if T.null afterSnippet
              ----------------------------------------------------------------
              -- 2a) We never found the ending "```" => incomplete block.
              ----------------------------------------------------------------
              then Left MalformedCodeBlock
              else
                -- 2b) Drop the ending fence from the remainder
                let afterSnippet' = T.drop (T.length "```") afterSnippet
                in
                  ----------------------------------------------------------------
                  -- 3) Check if there's another "```" (or "```agda") left in the text
                  --    after we've consumed one snippet. If so, multiple code blocks.
                  ----------------------------------------------------------------
                  if T.isInfixOf "```" afterSnippet'
                     then Left MultipleCodeBlocks
                     else
                       ----------------------------------------------------------------
                       -- 4) Exactly one fenced snippet found => success.
                       ----------------------------------------------------------------
                       Right (TrLLMAgdaSnippet snippet)
