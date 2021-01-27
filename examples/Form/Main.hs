{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Prelude hiding (div)
import Concur.Core (MultiAlternative (orr))
import Concur.Replica
import Control.Applicative (Alternative ((<|>)))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))

-- Form's show case
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input

waitFor n = liftIO $ threadDelay (n * 1_000_000)
-- * TODO input type="text2

-- The most simple form element, text input element!
inputTextWidget t = do
    ev <-
        orr
            [ div [] [ input [type_ "text", value t, onChange] ]
            , div [] [ text t ]
            ]
    inputTextWidget (extract ev)
  where
    extract BaseEvent{target = Target{targetValue}} = targetValue
-- * TODO input Checkbox

-- * TODO input Radiobutton

-- * TODO TextArea

-- * TODO Select

-- * TODO File

main :: IO ()
main = do
    runDefault 8080 "Form" $ do
        inputTextWidget "foo"
        pure ()
