module Main exposing (Msg, main)

import Browser exposing (Document)
import Browser.Navigation as Nav exposing (pushUrl, replaceUrl)
import Data.ApplicationData exposing (ApplicationData)
import File exposing (File)
import File.Download
import File.Select as Select
import Html.Styled exposing (Html, a, button, div, footer, nav, text, toUnstyled)
import Html.Styled.Attributes exposing (href)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Lazy exposing (lazy)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Page.Home as Home exposing (Msg)
import Page.Stats as Stats
import Parser exposing (encodedRoot, rootDecoder)
import Tailwind.Breakpoints exposing (lg)
import Tailwind.Utilities as Tw
import Task
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)


type alias Model =
    { page : Page
    , key : Nav.Key
    , applicationData : Maybe ApplicationData
    }


type Page
    = HomePage Home.Model
    | StatsPage Stats.Model
    | NotFound


type Route
    = Home
    | Stats


view : Model -> Document Msg
view model =
    let
        content =
            case model.applicationData of
                Nothing ->
                    text "not foudn"

                Just data ->
                    case model.page of
                        StatsPage stats ->
                            Stats.view data stats |> Html.Styled.map GotStatsMsg

                        HomePage home ->
                            Home.view data home
                                |> Html.Styled.map GotHomeMsg

                        NotFound ->
                            text "not found"
    in
    { title = "Climbing Portfolio"
    , body =
        List.map toUnstyled
            [ lazy viewHeader model.page
            , content
            , viewFooter
            ]
    }


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            div [ Html.Styled.Attributes.css [ Tw.flex, Tw.items_center, Tw.flex_shrink_0, Tw.text_white, Tw.mr_6 ] ] [ text "Climbing portfolio" ]

        links =
            div [ Html.Styled.Attributes.css [ Tw.w_full, Tw.block, Tw.flex_grow, lg [ Tw.flex, Tw.items_center, Tw.w_auto ] ] ]
                [ navLink Home { url = "/", caption = "Home" }
                , navLink Stats { url = "/stats", caption = "Stats" }
                ]

        navAttributes =
            [ Html.Styled.Attributes.css
                [ Tw.flex
                , Tw.items_center
                , Tw.justify_between
                , Tw.flex_wrap
                , Tw.bg_purple_400
                , Tw.p_6
                ]
            ]

        navLink : Route -> { url : String, caption : String } -> Html msg
        navLink route { url, caption } =
            a
                [ href url
                , Html.Styled.Attributes.css <|
                    [ Tw.block, Tw.mt_4, Tw.mr_4, Tw.text_purple_200, lg [ Tw.inline_block, Tw.mt_0 ] ]
                        ++ filterList [ ( Tw.underline, isActive { link = route, page = page }, Just Tw.no_underline ) ]
                ]
                [ text caption ]
    in
    nav navAttributes
        [ logo
        , links
        , button [ onClick JsonRequested ] [ text "Load JSON" ]
        , button [ onClick ExportRequested ] [ text "Save JSON" ]
        ]


filterList : List ( a, Bool, Maybe a ) -> List a
filterList list =
    case list of
        [] ->
            []

        ( style, b, maybeAlternative ) :: xs ->
            if b then
                style :: filterList xs

            else
                case maybeAlternative of
                    Nothing ->
                        filterList xs

                    Just alternative ->
                        alternative :: filterList xs


viewFooter : Html msg
viewFooter =
    footer []
        [ text "One is never alone with a rubber duck. -Douglas Adams"
        ]


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotStatsMsg Stats.Msg
    | GotHomeMsg Home.Msg
    | JsonRequested
    | JsonSelected File
    | JsonLoaded String
    | ExportRequested


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangedUrl url ->
            updateUrl url model

        GotHomeMsg homeMsg ->
            case model.page of
                HomePage home ->
                    case model.applicationData of
                        Nothing ->
                            ( model, Cmd.none )

                        Just applicationData ->
                            let
                                ( homeModel, homeCmd, newApplicationData ) =
                                    Home.update applicationData homeMsg home

                                ( newModel, newCmd ) =
                                    toHome model ( homeModel, homeCmd )
                            in
                            ( { newModel | applicationData = Just newApplicationData }, newCmd )

                _ ->
                    ( model, Cmd.none )

        GotStatsMsg statsMsg ->
            case model.page of
                StatsPage stats ->
                    toStats model (Stats.update statsMsg stats)

                _ ->
                    ( model, Cmd.none )

        JsonRequested ->
            ( model, Select.file [ "application/json" ] JsonSelected )

        JsonSelected file ->
            ( model, Task.perform JsonLoaded (File.toString file) )

        JsonLoaded content ->
            let
                result =
                    decodeString rootDecoder content
            in
            case result of
                Ok root ->
                    let
                        data : ApplicationData
                        data =
                            { routes = root.routes, ascents = root.ascents }
                    in
                    ( { model | applicationData = Just data }, replaceUrl model.key "" )

                Err _ ->
                    ( { model | applicationData = Nothing }, Cmd.none )

        ExportRequested ->
            case model.applicationData of
                Just data ->
                    let
                        result =
                            encode 4 <| encodedRoot data
                    in
                    ( model, File.Download.string "result.json" "application/json" result )

                Nothing ->
                    ( model, Cmd.none )


toHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
toHome model ( home, cmd ) =
    ( { model | page = HomePage home }, Cmd.map GotHomeMsg cmd )


toStats : Model -> ( Stats.Model, Cmd Stats.Msg ) -> ( Model, Cmd Msg )
toStats model ( stats, cmd ) =
    ( { model | page = StatsPage stats }, Cmd.map GotStatsMsg cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


isActive : { link : Route, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        ( Home, HomePage _ ) ->
            True

        ( Home, _ ) ->
            False

        ( Stats, StatsPage _ ) ->
            True

        ( Stats, _ ) ->
            False


init : Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    updateUrl url { page = NotFound, key = key, applicationData = Nothing }


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    let
        nothingPage =
            ( { model | page = NotFound }, Cmd.none )
    in
    case model.applicationData of
        Nothing ->
            nothingPage

        Just data ->
            case Parser.parse parser url of
                Just Home ->
                    Home.init
                        |> toHome model

                Just Stats ->
                    Stats.init
                        |> toStats model

                Nothing ->
                    nothingPage


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Stats (s "stats")
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = \_ -> init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
