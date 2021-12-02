module Main exposing (main)

import Browser exposing (Document)
import Init exposing (init)
import Message exposing (Msg)
import Model exposing (Model)
import Update exposing (update)
import View exposing (view)


main : Program () Model Msg
main =
    let
        aView : Model -> Document Msg
        aView model =
            { title = "Climbing Portfolio", body = view model }
    in
    Browser.application
        { init = \_ -> init
        , view = aView
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = Message.ClickedLink
        , onUrlChange = Message.ChangedUrl
        }
