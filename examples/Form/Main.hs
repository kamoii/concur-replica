{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Concur.Replica
import Control.Concurrent (threadDelay)

-- Form's show case
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input

waitFor n = threadDelay (n * 1_000_000)
-- * TODO input type="text2

-- The most simple form element, text input element!

-- TODO: input はあるのに, Alternative, display が import されていないのは微妙。
-- inputTextWidget t = do
--     t' <- input [type_ "text", value t, onChange]
--     waitFor 1.5 <|> display t'
--     pure ()

-- * TODO input Checkbox

-- * TODO input Radiobutton

-- * TODO TextArea

-- * TODO Select

-- * TODO File

main :: IO ()
main = do
    runDefault 8080 "Form" $ do
        -- inputTextWidget "foo"
        pure ()
