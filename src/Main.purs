module Main where

import Prelude

import Control.Monad.Eff.Console (error, log)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept)
import DOM.File.FileList as FileList
import DOM.HTML.HTMLInputElement (files)
import DOM.HTML.Types (readHTMLInputElement)
import DOM.HTML.URL (createObjectURL)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import React.Basic as R
import Unsafe.Coerce (unsafeCoerce)

type ExampleProps =
  {
  }

type ExampleState =
  { fileBlob :: Maybe String
  }

example :: R.ReactComponent ExampleProps
example = R.react
  { initialState
  , render
  }
  where
    initialState _ = { fileBlob: Nothing } :: ExampleState

    log' = unsafeCoerceEff <<< log
    error' = unsafeCoerceEff <<< error
    files' = unsafeCoerceEff <<< files

    onFileChange setState {currentTarget} = do
      case runExcept <<< readHTMLInputElement $ toForeign currentTarget of
        Left e ->
          error' $ "couldn't read input element: " <> show e
        Right input -> do
          file_ :: _ <- map (FileList.item 0) <$> files' input
          case file_ of
            Just f -> do
              blob <- createObjectURL f
              setState {blob}
            Nothing ->
              error' $ "couldn't get file from input"
          log' $ unsafeCoerce file_
          pure unit
      log' $ unsafeCoerce currentTarget

    render {} state setState =
      R.div
        {}
        [ R.h1 {} [ R.text "hello" ]
        , R.input
            { type: "file"
            , onChange: mkEffFn1 $ onFileChange setState
            }
        , R.audio
            { autoPlay: true
            , src: fromMaybe "" state.fileBlob
            }
            []
        ]
