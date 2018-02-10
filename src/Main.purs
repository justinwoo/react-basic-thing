module Main where

import Prelude

import Control.Monad.Eff.Console (error, log)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept)
import DOM.File.FileList as FileList
import DOM.HTML as Document
import DOM.HTML.HTMLInputElement as HTMLInputElement
import DOM.HTML.Types (readHTMLInputElement)
import DOM.HTML.URL (createObjectURL)
import DOM.HTML.Window as Window
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..), fromMaybe)
import React.Basic as R

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
    files' = unsafeCoerceEff <<< HTMLInputElement.files

    createObjectURL' f = unsafeCoerceEff $
      createObjectURL f =<< Window.url =<< Document.window

    onFileChange setState {currentTarget} = do
      case runExcept <<< readHTMLInputElement $ toForeign currentTarget of
        Left e ->
          error' $ "couldn't read input element: " <> show e
        Right input -> do
          filelist_ <- files' input
          case FileList.item 0 =<< filelist_ of
            Just f -> do
              fileBlob <- createObjectURL' f
              setState { fileBlob: Just fileBlob }
              pure unit
            Nothing ->
              error' $ "couldn't get file from input"

    render {} state setState =
      R.div
        { style: R.css
            { display: "flex"
            , flexDirection: "column"
            , width: "100%"
            }
        }
        [ R.h1 {} [ R.text "Welcome to React Basic with Audio" ]
        , R.input
            { type: "file"
            , onChange: mkEffFn1 $ onFileChange setState
            }
        , R.audio
            { autoPlay: true
            , src: fromMaybe "" state.fileBlob
            , controls: true
            }
            []
        ]
