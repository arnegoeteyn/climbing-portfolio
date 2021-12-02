module Main exposing (main)

import Browser exposing (Document)
import Command exposing (updateWithStorage)
import Init exposing (init)
import Message exposing (Msg)
import Model exposing (Model)
import Update exposing (update)
import View exposing (view)


main : Program String Model Msg
main =
    let
        aView : Model -> Document Msg
        aView model =
            { title = "Climbing Portfolio", body = view model }
    in
    Browser.application
        { init = init
        , view = aView
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = Message.ClickedLink
        , onUrlChange = Message.ChangedUrl
        }
