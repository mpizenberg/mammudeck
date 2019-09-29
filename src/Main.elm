----------------------------------------------------------------------
--
-- Main.elm
-- Mammudeck, a TweetDeck-like columnar interface to Mastodon/Pleroma.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------
--
-- Startup URLs
-- See `parseInitialPage`.
--
-- https://mamudeck.com/...
--
--   ?page=[splash|columns|api]
--   &api=[key in selectedRequestFromUrlDict]
--
----------------------------------------------------------------------


module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Char
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Deck
import Dialog
import Dict exposing (Dict)
import File exposing (File)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , div
        , h2
        , img
        , input
        , option
        , p
        , select
        , span
        , table
        , td
        , text
        , tr
        )
import Html.Attributes
    exposing
        ( alt
        , disabled
        , href
        , placeholder
        , selected
        , size
        , src
        , style
        , target
        , title
        , value
        )
import Html.Events exposing (keyCode, on, onClick, onInput)
import Html.Parser as Parser
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE exposing (Value)
import JsonTree exposing (TaggedValue(..))
import List.Extra as LE
import Mammudeck.EncodeDecode as MED
import Mammudeck.Types as Types
    exposing
        ( Feed
        , FeedElements(..)
        , FeedSet
        , FeedSetDefinition
        , FeedType(..)
        , FetchType(..)
        , RenderEnv
        , Style(..)
        )
import Markdown
import Mastodon.EncodeDecode as ED
import Mastodon.Entity
    exposing
        ( Account
        , App
        , AttachmentType(..)
        , Authorization
        , Entity(..)
        , Field
        , FilterContext(..)
        , NotificationType(..)
        , Privacy(..)
        , Visibility(..)
        , WrappedStatus(..)
        )
import Mastodon.Login as Login exposing (FetchAccountOrRedirect(..))
import Mastodon.Request as Request
    exposing
        ( Error(..)
        , Paging
        , RawRequest
        , Request(..)
        , Response
        , WhichGroups
        )
import PortFunnel.LocalStorage as LocalStorage
import PortFunnel.WebSocket as WebSocket
import PortFunnels exposing (FunnelDict, Handler(..), State)
import Task
import Time exposing (Posix, Zone)
import Url exposing (Url)
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as QP


type Started
    = NotStarted
    | StartedReadingModel
    | Started


type alias PagingInput =
    { max_id : String
    , since_id : String
    , min_id : String
    , limit : String
    }


emptyPagingInput : PagingInput
emptyPagingInput =
    { max_id = ""
    , since_id = ""
    , min_id = ""
    , limit = ""
    }


type Dialog
    = NoDialog
    | EditColumnsDialog


type alias FocusInput =
    { x : String
    , y : String
    }


type alias FilterInput =
    { phrase : String
    , context : List FilterContext
    , irreversible : Bool
    , whole_word : Bool
    , expires_in : String
    }


emptyFilterInput : FilterInput
emptyFilterInput =
    { phrase = ""
    , context = [ HomeContext ]
    , irreversible = False
    , whole_word = False
    , expires_in = ""
    }


encodeFilterInput : FilterInput -> Value
encodeFilterInput { phrase, context, irreversible, whole_word, expires_in } =
    JE.object
        [ ( "phrase", JE.string phrase )
        , ( "context", JE.list ED.encodeFilterContext context )
        , ( "irreversible", JE.bool irreversible )
        , ( "whole_word", JE.bool whole_word )
        , ( "expires_in", JE.string expires_in )
        ]


filterInputDecoder : Decoder FilterInput
filterInputDecoder =
    JD.succeed FilterInput
        |> required "phrase" JD.string
        |> required "context" (JD.list ED.filterContextDecoder)
        |> required "irreversible" JD.bool
        |> required "whole_word" JD.bool
        |> required "expires_in" JD.string


type alias RenderEnv =
    { loginServer : Maybe String
    , style : Style

    -- not persistent
    , windowSize : ( Int, Int )
    , here : Zone
    }


emptyRenderEnv : RenderEnv
emptyRenderEnv =
    { loginServer = Nothing
    , style = LightStyle
    , windowSize = ( 1024, 768 )
    , here = Time.utc
    }


type alias Model =
    { renderEnv : RenderEnv
    , page : Page
    , token : Maybe String
    , server : String

    -- Columns page state
    , feedSetDefinition : FeedSetDefinition

    -- API Explorer page state
    , username : String
    , accountId : String
    , accountIds : String
    , showMetadata : Bool
    , q : String
    , resolve : Bool
    , following : Bool
    , groupId : String
    , showReceived : Bool
    , showEntity : Bool
    , whichGroups : WhichGroups
    , followReblogs : Bool
    , onlyMedia : Bool
    , pinned : Bool
    , excludeReplies : Bool
    , excludeReblogs : Bool
    , pagingInput : PagingInput
    , local : Bool
    , hashtag : String
    , listId : String
    , smartPaging : Bool
    , showUpdateCredentials : Bool
    , statusId : String
    , useElmButtonNames : Bool
    , showPostStatus : Bool
    , excludedNotificationTypes : List NotificationType
    , notificationsAccountId : String
    , notificationId : String
    , muteNotifications : Bool
    , groupIds : String
    , offset : String
    , listTitle : String
    , filterId : String
    , filterInput : FilterInput
    , scheduledStatusId : String
    , userNameInput : String

    -- Non-persistent below here
    , initialPage : Maybe Page
    , dialog : Dialog
    , feedSet : FeedSet

    -- API Explorer state
    , altKeyDown : Bool
    , request : Maybe RawRequest
    , response : Maybe Value
    , entity : Maybe Entity
    , responseTree : Result JD.Error JsonTree.Node
    , responseState : JsonTree.State
    , entityTree : Result JD.Error JsonTree.Node
    , entityState : JsonTree.State
    , selectedKeyPath : JsonTree.KeyPath
    , selectedKeyValue : String
    , clipboardValue : String
    , clipboardCount : Int
    , metadata : Maybe Http.Metadata
    , savedModel : Maybe SavedModel
    , key : Key
    , url : Url
    , hideClientId : Bool
    , tokens : Dict String String
    , account : Maybe Account
    , displayName : String
    , note : String
    , fields : List Field
    , avatarFile : Maybe File
    , headerFile : Maybe File
    , locked : Bool
    , privacy : Privacy
    , sensitive : Bool
    , language : String
    , isAccountFollowed : Bool
    , status : String
    , in_reply_to_id : String
    , quote_of_id : String
    , spoiler_text : String
    , visibility : Maybe Visibility
    , scheduled_at : String
    , idempotencyKey : String
    , mediaFile : Maybe File
    , mediaDescription : String
    , mediaFocus : FocusInput
    , media_ids : String
    , media_id : String
    , expires_in : String
    , multiple : Bool
    , hide_totals : Bool
    , pollOptions : List String
    , groupTitle : String
    , groupDescription : String
    , groupCoverImage : Maybe File
    , reportComment : String
    , statusIds : String
    , forwardReport : Bool

    -- Not input state
    , msg : Maybe String
    , started : Started
    , funnelState : State
    , now : Maybe Posix
    }


type Page
    = SplashScreenPage
    | ColumnsPage


type Msg
    = Noop
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | GlobalMsg GlobalMsg
    | ColumnsUIMsg ColumnsUIMsg
    | ColumnsSendMsg ColumnsSendMsg


type GlobalMsg
    = WindowResize Int Int
    | Here Zone
    | Now Posix
    | SetPage String
    | OnKeyPress String
    | OnAltKey Bool
    | SetServer String
    | Process Value
    | SetLoginServer
    | Login
    | Logout
    | ReceiveRedirect (Result ( String, Error ) ( String, App, Cmd Msg ))
    | ReceiveAuthorization (Result ( String, Error ) ( String, Authorization, Account ))
    | ReceiveInstance (Result Error Response)
    | ReceiveFetchAccount (Result ( String, Error ) ( String, String, Account ))
    | ReceiveGetVerifyCredentials (Result Error Response)


type ColumnsUIMsg
    = ReloadAllColumns
    | ShowEditColumnsDialog
    | DismissDialog
    | AddFeedColumn FeedType
    | DeleteFeedColumn FeedType
    | UserNameInput String


type ColumnsSendMsg
    = ReceiveFeed FeedType (Result Error Response)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


keyDecoder : Decoder Msg
keyDecoder =
    JD.field "key" JD.string
        |> JD.map (GlobalMsg << OnKeyPress)


altKeyDecoder : Bool -> Decoder Msg
altKeyDecoder down =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                if key == "Alt" then
                    JD.succeed (GlobalMsg <| OnAltKey down)

                else
                    JD.fail "Not Alt key"
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PortFunnels.subscriptions (GlobalMsg << Process) model
        , Events.onResize (\w h -> GlobalMsg <| WindowResize w h)
        , if model.dialog /= NoDialog then
            Events.onKeyDown keyDecoder

          else
            Sub.batch
                [ Events.onKeyDown <| altKeyDecoder True
                , Events.onKeyUp <| altKeyDecoder False
                ]
        ]


emptyElement : String
emptyElement =
    "foo"


emptyUrl : Url
emptyUrl =
    { protocol = Url.Https
    , host = "example.com"
    , port_ = Nothing
    , path = "/" ++ emptyElement
    , query = Nothing
    , fragment = Nothing
    }


type alias CodeErrorState =
    { code : Maybe String
    , error : Maybe String
    , state : Maybe String
    }


parseQuery : String -> CodeErrorState
parseQuery queryString =
    let
        url =
            { emptyUrl | query = Just queryString }

        qp =
            QP.map3 CodeErrorState
                (QP.string "code")
                (QP.string "error")
                (QP.string "state")
    in
    Parser.parse (Parser.s emptyElement <?> qp) url
        |> Maybe.withDefault (CodeErrorState Nothing Nothing Nothing)


type CodeAndState
    = CodeAndState String (Maybe String)
    | CodeErrorAndState String (Maybe String)
    | NoCode


{-| This recognizes `?code=<code>&state=<state>` or `?error=<error>&state=<state>`

in the URL from the redirect from authentication.

-}
receiveCodeAndState : Url -> CodeAndState
receiveCodeAndState url =
    case url.query of
        Nothing ->
            NoCode

        Just q ->
            case parseQuery q of
                { code, error, state } ->
                    case code of
                        Just cod ->
                            case state of
                                Just _ ->
                                    CodeAndState cod state

                                Nothing ->
                                    CodeErrorAndState "Missing state with code" code

                        Nothing ->
                            case error of
                                Just err ->
                                    CodeErrorAndState err state

                                Nothing ->
                                    NoCode


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init value url key =
    let
        hideClientId =
            case JD.decodeValue JD.bool value of
                Err _ ->
                    False

                Ok hide ->
                    hide

        ( code, state, msg ) =
            case receiveCodeAndState url of
                CodeAndState cod stat ->
                    ( Just cod, stat, Nothing )

                CodeErrorAndState m stat ->
                    ( Nothing, stat, Just m )

                NoCode ->
                    ( Nothing, Nothing, Nothing )

        initialPage =
            Nothing
    in
    { renderEnv = emptyRenderEnv
    , page = SplashScreenPage
    , token = Nothing
    , server = ""
    , feedSetDefinition = Types.emptyFeedSetDefinition
    , username = ""
    , accountId = ""
    , accountIds = ""
    , showMetadata = False
    , q = ""
    , resolve = False
    , following = False
    , groupId = ""
    , showReceived = True
    , showEntity = False
    , whichGroups = Request.MemberGroups
    , followReblogs = True
    , onlyMedia = False
    , pinned = False
    , excludeReplies = False
    , excludeReblogs = False
    , pagingInput = emptyPagingInput
    , local = False
    , hashtag = ""
    , listId = ""
    , smartPaging = False
    , showUpdateCredentials = False
    , showPostStatus = False
    , statusId = ""
    , useElmButtonNames = False
    , excludedNotificationTypes = []
    , notificationsAccountId = ""
    , notificationId = ""
    , muteNotifications = True
    , groupIds = ""
    , offset = ""
    , listTitle = ""
    , filterId = ""
    , filterInput = emptyFilterInput
    , scheduledStatusId = ""
    , userNameInput = ""

    -- Non-persistent below here
    , initialPage = initialPage
    , dialog = NoDialog
    , feedSet = Types.emptyFeedSet
    , altKeyDown = False
    , request = Nothing
    , response = Nothing
    , entity = Nothing
    , responseTree = emptyJsonTree
    , responseState = JsonTree.defaultState
    , entityTree = emptyJsonTree
    , entityState = JsonTree.defaultState
    , selectedKeyPath = ""
    , selectedKeyValue = ""
    , clipboardValue = ""
    , clipboardCount = 0
    , metadata = Nothing
    , savedModel = Nothing
    , key = key
    , url = url
    , hideClientId = hideClientId
    , tokens = Dict.empty
    , account = Nothing
    , displayName = ""
    , note = ""
    , fields = []
    , avatarFile = Nothing
    , headerFile = Nothing
    , locked = False
    , privacy = PublicPrivacy
    , sensitive = False
    , language = ""
    , isAccountFollowed = False
    , status = ""
    , in_reply_to_id = ""
    , quote_of_id = ""
    , spoiler_text = ""
    , visibility = Nothing
    , scheduled_at = ""
    , idempotencyKey = ""
    , mediaFile = Nothing
    , mediaDescription = ""
    , mediaFocus = { x = "", y = "" }
    , media_ids = ""
    , media_id = ""
    , expires_in = ""
    , multiple = False
    , hide_totals = False
    , pollOptions = [ "", "" ]
    , groupTitle = ""
    , groupDescription = ""
    , groupCoverImage = Nothing
    , reportComment = ""
    , statusIds = ""
    , forwardReport = True
    , msg = msg
    , started = NotStarted
    , funnelState = initialFunnelState
    , now = Nothing
    }
        -- As soon as the localStorage module reports in,
        -- we'll load the saved model,
        -- and then all the saved tokens.
        -- See `storageHandler` below, `get pk.model`.
        |> withCmds
            [ Navigation.replaceUrl key url.path
            , case ( code, state ) of
                ( Just cod, Just st ) ->
                    Login.getTokenTask { code = cod, state = st }
                        |> Task.attempt (GlobalMsg << ReceiveAuthorization)

                _ ->
                    Cmd.none
            , Task.perform getViewport Dom.getViewport
            , Task.perform (GlobalMsg << Here) Time.here
            , Task.perform (GlobalMsg << Now) Time.now
            ]


getViewport : Viewport -> Msg
getViewport viewport =
    let
        vp =
            viewport.viewport
    in
    GlobalMsg <| WindowResize (round vp.width) (round vp.height)


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    let
        mdl =
            { model
                | started =
                    if
                        LocalStorage.isLoaded state.storage
                            && (model.started == NotStarted)
                    then
                        StartedReadingModel

                    else
                        model.started
            }

        cmd =
            if
                (mdl.started == StartedReadingModel)
                    && (model.started == NotStarted)
            then
                Cmd.batch
                    [ get pk.model
                    , listKeysLabeled pk.token (pk.token ++ ".")
                    ]

            else
                Cmd.none
    in
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            handleGetResponse label key value mdl

        LocalStorage.ListKeysResponse { label, prefix, keys } ->
            handleListKeysResponse label prefix keys model

        _ ->
            mdl |> withCmd cmd


getInstance : Model -> Cmd Msg
getInstance model =
    let
        serverInfo =
            { server = model.server
            , token = Nothing
            }
    in
    Request.serverRequest (\_ -> GlobalMsg << ReceiveInstance)
        []
        serverInfo
        ()
        (InstanceRequest Request.GetInstance)


getVerifyCredentials : Model -> Cmd Msg
getVerifyCredentials model =
    case model.renderEnv.loginServer of
        Nothing ->
            Cmd.none

        Just server ->
            case model.token of
                Nothing ->
                    Cmd.none

                Just token ->
                    Request.serverRequest (\_ -> GlobalMsg << ReceiveGetVerifyCredentials)
                        []
                        { server = server
                        , token = Just token
                        }
                        ()
                    <|
                        AccountsRequest Request.GetVerifyCredentials


handleListKeysResponse : Maybe String -> String -> List String -> Model -> ( Model, Cmd Msg )
handleListKeysResponse maybeLabel _ keys model =
    case maybeLabel of
        Nothing ->
            model |> withNoCmd

        Just label ->
            -- label will be pk.token,
            -- but we won't care about that until the value comes in
            -- to handleGetResponse below.
            model |> withCmds (List.map (getLabeled label) keys)


handleGetModel : Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetModel maybeValue model =
    let
        ( mdl, cmd ) =
            handleGetModelInternal maybeValue model

        page =
            mdl.initialPage
    in
    { mdl
        | page =
            case page of
                Nothing ->
                    mdl.page

                Just p ->
                    p
    }
        |> withCmds
            [ cmd
            , getFeedSetDefinition
            ]


handleGetModelInternal : Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetModelInternal maybeValue model =
    case maybeValue of
        Nothing ->
            { model
                | started = Started
                , msg = Nothing
            }
                |> withNoCmd

        Just value ->
            case JD.decodeValue savedModelDecoder value of
                Err err ->
                    { model
                        | started = Started
                        , msg =
                            Just <|
                                Debug.log "Error decoding SavedModel"
                                    (JD.errorToString err)
                    }
                        |> withNoCmd

                Ok savedModel ->
                    let
                        mdl =
                            savedModelToModel savedModel model
                    in
                    { mdl
                        | started = Started
                        , msg = Nothing
                    }
                        |> withCmd
                            (if mdl.renderEnv.loginServer == Nothing then
                                Task.perform (GlobalMsg << SetServer) <|
                                    Task.succeed mdl.server

                             else
                                getVerifyCredentials mdl
                            )


handleGetFeedSetDefinition : Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetFeedSetDefinition maybeValue model =
    let
        feedSetDefinition =
            case maybeValue of
                Nothing ->
                    Types.defaultFeedSetDefinition

                Just value ->
                    case JD.decodeValue MED.feedSetDefinitionDecoder value of
                        Err _ ->
                            Types.defaultFeedSetDefinition

                        Ok fsd ->
                            fsd

        feedSet =
            Types.feedSetDefinitionToFeedSet feedSetDefinition
    in
    { model
        | feedSetDefinition = feedSetDefinition
        , feedSet = feedSet
    }
        |> withCmd
            (if model.page == ColumnsPage then
                Task.perform identity <|
                    Task.succeed (ColumnsUIMsg ReloadAllColumns)

             else
                Cmd.none
            )


handleGetToken : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetToken key value model =
    case JD.decodeValue JD.string value of
        Err err ->
            let
                _ =
                    Debug.log ("Error decoding " ++ key) err
            in
            model |> withNoCmd

        Ok token ->
            let
                tokens =
                    model.tokens

                server =
                    Debug.log "Received token for server" <|
                        tokenStorageKeyServer key
            in
            { model | tokens = Dict.insert server token tokens }
                |> withNoCmd


handleGetResponse : Maybe String -> String -> Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetResponse maybeLabel key maybeValue model =
    case maybeLabel of
        Nothing ->
            if key == pk.model then
                handleGetModel maybeValue model

            else if key == pk.feedSetDefinition then
                handleGetFeedSetDefinition maybeValue model

            else
                model |> withNoCmd

        Just label ->
            case maybeValue of
                Nothing ->
                    model |> withNoCmd

                Just value ->
                    if label == pk.token then
                        handleGetToken key value model

                    else
                        model |> withNoCmd


socketHandler : WebSocket.Response -> State -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            { mdl | funnelState = state }
    in
    case response of
        WebSocket.ErrorResponse error ->
            case error of
                WebSocket.SocketAlreadyOpenError _ ->
                    socketHandler
                        (WebSocket.ConnectedResponse { key = "", description = "" })
                        state
                        model

                _ ->
                    { model | msg = Just <| WebSocket.errorToString error }
                        |> withNoCmd

        WebSocket.MessageReceivedResponse _ ->
            model |> withNoCmd

        WebSocket.ClosedResponse _ ->
            model |> withNoCmd

        WebSocket.ConnectedResponse _ ->
            model |> withNoCmd

        _ ->
            model |> withNoCmd


emptyJsonTree : Result JD.Error JsonTree.Node
emptyJsonTree =
    JsonTree.parseString "[]"


updateJsonTrees : Model -> Model
updateJsonTrees model =
    let
        parse : Maybe Value -> ( Result JD.Error JsonTree.Node, JsonTree.State )
        parse value =
            case value of
                Nothing ->
                    ( emptyJsonTree, JsonTree.defaultState )

                Just v ->
                    let
                        result =
                            JsonTree.parseValue v
                    in
                    ( result
                    , case result of
                        Err _ ->
                            JsonTree.defaultState

                        Ok root ->
                            JsonTree.collapseToDepth 1 root JsonTree.defaultState
                    )

        ( responseTree, responseState ) =
            parse model.response

        ( entityTree, entityState ) =
            case model.entity of
                Nothing ->
                    parse Nothing

                Just entity ->
                    parse (Just <| ED.encodeEntity entity)
    in
    { model
        | responseTree = responseTree
        , responseState = responseState
        , entityTree = entityTree
        , entityState = entityState
        , selectedKeyPath = ""
        , selectedKeyValue = ""
    }


updatePatchCredentialsInputs : Model -> Model
updatePatchCredentialsInputs mdl =
    let
        model =
            updateJsonTrees mdl
    in
    case model.account of
        Nothing ->
            { model
                | displayName = ""
                , note = ""
                , locked = False
                , privacy = PublicPrivacy
                , sensitive = False
                , language = ""
            }

        Just account ->
            let
                ( ( privacy, sensitive, language ), ( note, fields ) ) =
                    case account.source of
                        Nothing ->
                            ( ( PublicPrivacy, False, "" )
                            , ( "", [] )
                            )

                        Just source ->
                            ( ( source.privacy
                              , source.sensitive
                              , Maybe.withDefault "" source.language
                              )
                            , ( source.note, source.fields )
                            )

                locked =
                    account.locked
            in
            { model
                | displayName = account.display_name
                , note = note
                , fields = extendFields fields
                , locked = locked
                , privacy = privacy
                , sensitive = sensitive
                , language = language
            }


extendFields : List Field -> List Field
extendFields fields =
    List.concat
        [ fields
        , List.repeat (4 - List.length fields) emptyField
        ]


emptyField : Field
emptyField =
    { name = ""
    , value = ""
    , verified_at = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( model2, cmd ) =
            updateInternal msg model

        savedModel =
            modelToSavedModel model2

        needsSaving =
            if model2.started /= Started then
                False

            else
                case model2.savedModel of
                    Nothing ->
                        True

                    Just sm ->
                        savedModel /= sm
    in
    { model2
        | savedModel =
            if needsSaving then
                Just savedModel

            else
                model2.savedModel
    }
        |> withCmds
            [ cmd
            , if needsSaving then
                put pk.model (Just <| encodeSavedModel savedModel)

              else
                Cmd.none
            ]


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    case msg of
        Noop ->
            model |> withNoCmd

        OnUrlRequest _ ->
            model |> withNoCmd

        OnUrlChange _ ->
            model |> withNoCmd

        GlobalMsg m ->
            globalMsg m model

        ColumnsUIMsg m ->
            columnsUIMsg m model

        ColumnsSendMsg m ->
            columnsSendMsg m model


{-| Process global messages.
-}
globalMsg : GlobalMsg -> Model -> ( Model, Cmd Msg )
globalMsg msg model =
    let
        renderEnv =
            model.renderEnv
    in
    case msg of
        WindowResize w h ->
            { model
                | renderEnv =
                    { renderEnv | windowSize = Debug.log "windowSize" ( w, h ) }
            }
                |> withNoCmd

        Here zone ->
            { model
                | renderEnv =
                    { renderEnv | here = zone }
            }
                |> withNoCmd

        Now posix ->
            { model | now = Just posix }
                |> withNoCmd

        SetPage pageString ->
            let
                page =
                    stringToPage pageString
            in
            { model | page = page }
                |> withCmd
                    (if page == ColumnsPage && feedsNeedLoading model then
                        Task.perform identity <|
                            Task.succeed (ColumnsUIMsg ReloadAllColumns)

                     else
                        Cmd.none
                    )

        OnKeyPress key ->
            { model
                | dialog =
                    if key == "Escape" then
                        NoDialog

                    else
                        model.dialog
            }
                |> withNoCmd

        OnAltKey isDown ->
            { model
                | altKeyDown = isDown
            }
                |> withNoCmd

        SetServer server ->
            let
                mdl =
                    { model | server = server }
            in
            mdl
                |> withCmd
                    (if String.contains "." server then
                        getInstance mdl

                     else
                        Cmd.none
                    )

        Process value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    model.funnelState
                    model
            of
                Err error ->
                    { model | msg = Just <| Debug.toString error }
                        |> withNoCmd

                Ok res ->
                    res

        SetLoginServer ->
            { model
                | msg = Nothing
                , renderEnv =
                    { renderEnv | loginServer = Nothing }
                , request = Nothing
                , response = Nothing
                , entity = Nothing
                , metadata = Nothing
                , selectedKeyPath = ""
                , selectedKeyValue = ""
            }
                |> withNoCmd

        Login ->
            let
                url =
                    model.url

                sau =
                    { client_name = "mammudeck"
                    , server = model.server
                    , applicationUri =
                        { url
                            | fragment = Nothing
                            , query = Nothing
                        }
                            |> Url.toString
                    }
            in
            case Login.loginTask sau <| Dict.get model.server model.tokens of
                Redirect task ->
                    ( model, Task.attempt (GlobalMsg << ReceiveRedirect) task )

                FetchAccount task ->
                    ( model, Task.attempt (GlobalMsg << ReceiveFetchAccount) task )

        Logout ->
            case model.renderEnv.loginServer of
                Nothing ->
                    model |> withNoCmd

                Just server ->
                    { model
                        | server = server
                        , account = Nothing
                        , tokens = Dict.remove server model.tokens
                        , token = Nothing
                        , request = Nothing
                        , response = Nothing
                        , entity = Nothing
                        , metadata = Nothing
                        , selectedKeyPath = ""
                        , selectedKeyValue = ""
                        , msg = Nothing
                    }
                        |> updatePatchCredentialsInputs
                        |> withCmd (putToken server Nothing)

        ReceiveRedirect result ->
            case result of
                Err ( _, err ) ->
                    ( { model | msg = Just <| Debug.toString err }
                    , Cmd.none
                    )

                Ok ( _, _, cmd ) ->
                    { model | msg = Nothing }
                        |> withCmd cmd

        ReceiveAuthorization result ->
            case result of
                Err ( _, err ) ->
                    ( { model | msg = Just <| Debug.toString err }
                    , Cmd.none
                    )

                Ok ( server, authorization, account ) ->
                    let
                        ( mdl, cmd ) =
                            saveAuthorization server authorization model

                        serverInfo =
                            { server = server
                            , token = Just authorization.token
                            }

                        mdl2 =
                            { mdl
                                | msg = Nothing
                                , token = Just authorization.token
                                , renderEnv =
                                    { renderEnv | loginServer = Just server }
                                , account = Just account
                                , request =
                                    -- Fake the request
                                    Just <|
                                        Request.requestToRawRequest []
                                            serverInfo
                                            (AccountsRequest Request.GetVerifyCredentials)
                                , response = Just account.v
                                , entity = Just <| AccountEntity account
                            }
                                |> updatePatchCredentialsInputs
                    in
                    ( mdl2, cmd )

        ReceiveFetchAccount result ->
            case result of
                Err error ->
                    { model | msg = Just <| Debug.toString error }
                        |> withNoCmd

                Ok ( loginServer, token, account ) ->
                    let
                        serverInfo =
                            { server = loginServer
                            , token = Just token
                            }

                        request =
                            -- Fake the request
                            Request.requestToRawRequest []
                                serverInfo
                                (AccountsRequest Request.GetVerifyCredentials)

                        mdl =
                            { model
                                | msg = Nothing
                                , server = loginServer
                                , renderEnv =
                                    { renderEnv | loginServer = Just loginServer }
                                , token = Just token
                                , account = Just account
                                , request = Just request
                                , response = Just account.v
                                , entity = Just <| AccountEntity account
                            }
                                |> updatePatchCredentialsInputs
                    in
                    ( mdl, Cmd.none )

        ReceiveInstance result ->
            case result of
                Err _ ->
                    -- We'll get lots of errors, for non-existant domains
                    model |> withNoCmd

                Ok response ->
                    case response.entity of
                        InstanceEntity instance ->
                            { model
                                | msg = Nothing
                                , request = Just response.rawRequest
                                , metadata = Just response.metadata
                                , response = Just instance.v
                                , entity = Just response.entity
                            }
                                |> updateJsonTrees
                                |> withNoCmd

                        _ ->
                            model |> withNoCmd

        ReceiveGetVerifyCredentials result ->
            case result of
                Err error ->
                    { model | msg = Just <| Debug.toString error }
                        |> withNoCmd

                Ok response ->
                    case response.entity of
                        AccountEntity account ->
                            let
                                mdl =
                                    { model
                                        | msg = Nothing
                                        , request = Just response.rawRequest
                                        , metadata = Just response.metadata
                                        , response = Just account.v
                                        , entity = Just response.entity
                                        , account = Just account
                                    }
                                        |> updatePatchCredentialsInputs
                            in
                            ( mdl, Cmd.none )

                        _ ->
                            model |> withNoCmd


feedsNeedLoading : Model -> Bool
feedsNeedLoading model =
    model.account
        /= Nothing
        && feedSetIsEmpty model.feedSet


feedSetIsEmpty : FeedSet -> Bool
feedSetIsEmpty feedSet =
    List.map feedLength feedSet.feeds
        |> List.filter ((/=) 0)
        |> (==) []


feedLength : Feed -> Int
feedLength feed =
    case feed.elements of
        StatusElements list ->
            List.length list

        NotificationElements list ->
            List.length list

        AccountElements list ->
            List.length list

        ConversationsElements list ->
            List.length list

        ResultsElements list ->
            List.length list


{-| Process UI messages from the columns page.

These change the Model, but don't send anything over the wire to any instances.

-}
columnsUIMsg : ColumnsUIMsg -> Model -> ( Model, Cmd Msg )
columnsUIMsg msg model =
    case msg of
        ReloadAllColumns ->
            let
                getFeed : Feed -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
                getFeed feed ( mdl, cmds ) =
                    let
                        ( mdl2, cmd ) =
                            reloadFeed feed mdl
                    in
                    ( mdl2, Cmd.batch [ cmd, cmds ] )
            in
            List.foldr getFeed ( model, Cmd.none ) model.feedSet.feeds

        ShowEditColumnsDialog ->
            { model | dialog = EditColumnsDialog }
                |> withNoCmd

        DismissDialog ->
            { model | dialog = NoDialog }
                |> withNoCmd

        AddFeedColumn feedType ->
            addFeedType (fillinFeedType feedType model) model

        DeleteFeedColumn feedType ->
            deleteFeedType feedType model

        UserNameInput userNameInput ->
            { model | userNameInput = userNameInput }
                |> withNoCmd


fillinFeedType : FeedType -> Model -> FeedType
fillinFeedType feedType model =
    case feedType of
        UserFeed _ ->
            let
                ( username, server ) =
                    case String.split "@" model.userNameInput of
                        [] ->
                            -- Can't happen
                            ( "", "" )

                        [ name ] ->
                            ( name, "" )

                        name :: s :: _ ->
                            ( name, s )
            in
            UserFeed
                { username = username
                , server = server
                , flags = Nothing
                }

        _ ->
            feedType


deleteFeedType : FeedType -> Model -> ( Model, Cmd Msg )
deleteFeedType feedType model =
    let
        feedSetDefinition =
            model.feedSetDefinition

        feedTypes =
            feedSetDefinition.feedTypes

        newFeedSetDefinition =
            { feedSetDefinition
                | feedTypes =
                    List.filter ((/=) feedType) feedTypes
            }

        feedSet =
            model.feedSet

        newFeedSet =
            { feedSet
                | feeds =
                    List.filter (.feedType >> (/=) feedType) feedSet.feeds
            }
    in
    { model
        | feedSetDefinition = newFeedSetDefinition
        , feedSet = newFeedSet
    }
        |> withCmd (putFeedSetDefinition newFeedSetDefinition)


addFeedType : FeedType -> Model -> ( Model, Cmd Msg )
addFeedType feedType model =
    let
        feedSetDefinition =
            model.feedSetDefinition

        feedTypes =
            feedSetDefinition.feedTypes

        newFeedSetDefinition =
            { feedSetDefinition
                | feedTypes = List.append feedTypes [ feedType ]
            }

        feedSet =
            model.feedSet

        newFeed =
            { feedType = feedType
            , elements = Types.feedTypeToElements feedType
            }

        newFeedSet =
            { feedSet
                | feeds =
                    List.append feedSet.feeds [ newFeed ]
            }
    in
    { model
        | feedSetDefinition = newFeedSetDefinition
        , feedSet = newFeedSet
    }
        |> reloadFeed newFeed
        |> addCmd (putFeedSetDefinition newFeedSetDefinition)


{-| TODO:

This needs to be sent to `server`, not `model.renderEnv.loginServer`.
Or maybe that should be a parameter.

-}
startReloadUserFeed : Types.UserFeedParams -> Request
startReloadUserFeed params =
    AccountsRequest <|
        Request.GetSearchAccounts
            { q = params.username
            , limit = Nothing
            , resolve = False
            , following = False
            }


{-| This processes the result of the `GetSearchAccounts` request above.
-}
continueReloadUserFeed : FeedType -> List Account -> Model -> ( Model, Cmd Msg )
continueReloadUserFeed feedType accounts model =
    case feedType of
        UserFeed { username, server, flags } ->
            let
                userAtServer =
                    if (server == "") || (Just server == model.renderEnv.loginServer) then
                        username

                    else
                        username ++ "@" ++ server
            in
            case LE.find (.acct >> (==) userAtServer) accounts of
                Nothing ->
                    model |> withNoCmd

                Just { id } ->
                    let
                        ( ( only_media, pinned ), ( exclude_replies, exclude_reblogs ) ) =
                            case flags of
                                Nothing ->
                                    ( ( False, False ), ( False, False ) )

                                Just flgs ->
                                    ( ( flgs.only_media, flgs.pinned )
                                    , ( not flgs.replies, not flgs.reblogs )
                                    )

                        req =
                            AccountsRequest <|
                                Request.GetStatuses
                                    { id = id
                                    , only_media = only_media
                                    , pinned = pinned
                                    , exclude_replies = exclude_replies
                                    , exclude_reblogs = exclude_reblogs
                                    , paging = Nothing
                                    }
                    in
                    sendGeneralRequest (ColumnsSendMsg << ReceiveFeed feedType)
                        req
                        model

        _ ->
            model |> withNoCmd


reloadFeed : Feed -> Model -> ( Model, Cmd Msg )
reloadFeed { feedType } model =
    let
        request =
            case feedType of
                HomeFeed ->
                    Just <|
                        TimelinesRequest <|
                            Request.GetHomeTimeline { paging = Nothing }

                UserFeed params ->
                    Just <| startReloadUserFeed params

                PublicFeed { flags } ->
                    Just <|
                        TimelinesRequest <|
                            Request.GetPublicTimeline <|
                                -- Need to handle paging
                                case flags of
                                    Nothing ->
                                        { local = True
                                        , only_media = False
                                        , paging = Nothing
                                        }

                                    Just { local, only_media } ->
                                        { local = local
                                        , only_media = only_media
                                        , paging = Nothing
                                        }

                NotificationFeed { accountId, exclusions } ->
                    Just <|
                        NotificationsRequest <|
                            Request.GetNotifications
                                { paging = Nothing
                                , exclude_types = exclusions
                                , account_id = accountId
                                }

                _ ->
                    Nothing
    in
    case request of
        Nothing ->
            model |> withNoCmd

        Just req ->
            sendGeneralRequest (ColumnsSendMsg << ReceiveFeed feedType)
                req
                model


{-| Process Requests sent from the columns page.

These send requests over the wire to instances.

-}
columnsSendMsg : ColumnsSendMsg -> Model -> ( Model, Cmd Msg )
columnsSendMsg msg model =
    case msg of
        ReceiveFeed feedType result ->
            let
                ( mdl, cmd ) =
                    receiveResponse result model
            in
            case mdl.msg of
                Just _ ->
                    mdl |> withCmd cmd

                Nothing ->
                    case mdl.entity of
                        Nothing ->
                            mdl |> withCmd cmd

                        Just e ->
                            let
                                elements =
                                    case e of
                                        StatusListEntity statuses ->
                                            Just <| StatusElements statuses

                                        NotificationListEntity notifications ->
                                            Just <| NotificationElements notifications

                                        AccountListEntity accounts ->
                                            Just <| AccountElements accounts

                                        _ ->
                                            Nothing

                                feedSet =
                                    mdl.feedSet

                                ( feeds, ( mdl2, cmd2 ) ) =
                                    case elements of
                                        Nothing ->
                                            ( feedSet.feeds, ( mdl, Cmd.none ) )

                                        Just elem ->
                                            case elem of
                                                AccountElements accounts ->
                                                    ( feedSet.feeds
                                                    , continueReloadUserFeed
                                                        feedType
                                                        accounts
                                                        mdl
                                                    )

                                                _ ->
                                                    ( LE.updateIf
                                                        (\feed ->
                                                            feedType == feed.feedType
                                                        )
                                                        (\feed ->
                                                            { feed | elements = elem }
                                                        )
                                                        feedSet.feeds
                                                    , ( mdl, Cmd.none )
                                                    )
                            in
                            { mdl2
                                | feedSet =
                                    { feedSet | feeds = feeds }
                            }
                                |> withCmds [ cmd, cmd2 ]


receiveResponse : Result Error Response -> Model -> ( Model, Cmd Msg )
receiveResponse result model =
    case result of
        Err err ->
            let
                threeStrikes =
                    ( Nothing, Nothing, Nothing )

                ( msg, ( response, entity, metadata ) ) =
                    case err of
                        BadUrl url ->
                            ( "BadUrl: " ++ url, threeStrikes )

                        Timeout ->
                            ( "Timeout", threeStrikes )

                        NetworkError ->
                            ( "Network Error", threeStrikes )

                        BadStatus meta status ->
                            ( "Bad status: " ++ status, ( Nothing, Nothing, Just meta ) )

                        BadBody meta jderr json ->
                            let
                                res =
                                    case JD.decodeString JD.value json of
                                        Err _ ->
                                            Nothing

                                        Ok v ->
                                            Just v

                                m =
                                    "BadBody: " ++ decodeErrorToString jderr
                            in
                            ( m, ( res, Nothing, Just meta ) )
            in
            { model
                | msg = Just msg
                , response = response
                , entity = entity
                , metadata = metadata
                , selectedKeyPath = ""
                , selectedKeyValue = ""
            }
                |> updateJsonTrees
                |> withNoCmd

        Ok response ->
            let
                mdl =
                    applyResponseSideEffects response model
            in
            { mdl
                | msg = Nothing
                , metadata = Just response.metadata
                , response = Just <| ED.entityValue response.entity
                , entity = Just response.entity
            }
                |> updateJsonTrees
                |> withNoCmd


decodeErrorToString : JD.Error -> String
decodeErrorToString error =
    case error of
        JD.Field field err ->
            "Error on field \"" ++ field ++ "\": " ++ decodeErrorToString err

        JD.Index idx err ->
            "Error on index " ++ String.fromInt idx ++ "\": " ++ decodeErrorToString err

        JD.OneOf errors ->
            case errors of
                [] ->
                    "OneOf []"

                err :: _ ->
                    decodeErrorToString err

        JD.Failure fail _ ->
            fail


applyResponseSideEffects : Response -> Model -> Model
applyResponseSideEffects response model =
    case response.request of
        AccountsRequest Request.GetVerifyCredentials ->
            case response.entity of
                AccountEntity _ ->
                    updatePatchCredentialsInputs model

                _ ->
                    model

        AccountsRequest (Request.GetAccountByUsername _) ->
            case response.entity of
                AccountEntity { id } ->
                    { model | accountId = id }

                _ ->
                    model

        AccountsRequest (Request.PostFollow _) ->
            { model | isAccountFollowed = True }

        AccountsRequest (Request.PostUnfollow _) ->
            { model | isAccountFollowed = False }

        AccountsRequest (Request.PatchUpdateCredentials _) ->
            case response.entity of
                AccountEntity account ->
                    { model
                        | account = Just account
                        , avatarFile = Nothing
                        , headerFile = Nothing
                    }
                        |> updatePatchCredentialsInputs

                _ ->
                    model

        AccountsRequest (Request.GetStatuses { paging }) ->
            statusSmartPaging response.entity paging model

        FiltersRequest (Request.PostFilter _) ->
            case response.entity of
                FilterEntity { id } ->
                    { model | filterId = id }

                _ ->
                    model

        GroupsRequest (Request.PostGroup _) ->
            case response.entity of
                GroupEntity { id } ->
                    { model
                        | groupId = id
                        , groupTitle = ""
                        , groupDescription = ""
                        , groupCoverImage = Nothing
                    }

                _ ->
                    model

        GroupsRequest (Request.PutGroup _) ->
            { model
                | groupTitle = ""
                , groupDescription = ""
                , groupCoverImage = Nothing
            }

        NotificationsRequest (Request.GetNotifications { paging }) ->
            notificationsSmartPaging response.entity paging model

        StatusesRequest (Request.PostStatus _) ->
            case response.entity of
                StatusEntity { id } ->
                    { model
                        | statusId = id
                        , status = ""
                        , in_reply_to_id = ""
                        , quote_of_id = ""
                        , spoiler_text = ""
                        , scheduled_at = ""
                        , idempotencyKey = ""
                        , pollOptions = [ "", "" ]
                    }

                _ ->
                    model

        MediaAttachmentsRequest mediaReq ->
            case response.entity of
                AttachmentEntity { id } ->
                    let
                        mdl =
                            case mediaReq of
                                Request.PostMedia _ ->
                                    { model
                                        | media_id = id
                                        , media_ids =
                                            splitMediaIds model.media_ids
                                                |> (\ids ->
                                                        List.concat [ ids, [ id ] ]
                                                   )
                                                |> String.join ","
                                    }

                                _ ->
                                    model
                    in
                    { mdl
                        | mediaFile = Nothing
                        , mediaDescription = ""
                        , mediaFocus = { x = "", y = "" }
                    }

                _ ->
                    model

        TimelinesRequest req ->
            case req of
                Request.GetHomeTimeline { paging } ->
                    statusSmartPaging response.entity paging model

                Request.GetConversations { paging } ->
                    case response.entity of
                        ConversationListEntity conversations ->
                            smartPaging conversations .id paging model

                        _ ->
                            model

                Request.GetPublicTimeline { paging } ->
                    statusSmartPaging response.entity paging model

                Request.GetTagTimeline { paging } ->
                    statusSmartPaging response.entity paging model

                Request.GetListTimeline { paging } ->
                    statusSmartPaging response.entity paging model

                Request.GetGroupTimeline { paging } ->
                    statusSmartPaging response.entity paging model

        _ ->
            model


splitMediaIds : String -> List String
splitMediaIds string =
    let
        s =
            String.trim string
    in
    if s == "" then
        []

    else
        String.split "," s
            |> List.map String.trim


statusSmartPaging : Entity -> Maybe Paging -> Model -> Model
statusSmartPaging entity paging model =
    case entity of
        StatusListEntity statuses ->
            smartPaging statuses .id paging model

        _ ->
            model


notificationsSmartPaging : Entity -> Maybe Paging -> Model -> Model
notificationsSmartPaging entity paging model =
    case entity of
        NotificationListEntity notifications ->
            smartPaging notifications .id paging model

        _ ->
            model


smartPaging : List a -> (a -> String) -> Maybe Paging -> Model -> Model
smartPaging entities getid paging model =
    let
        ( limit, ( max_id, min_id, since_id ) ) =
            case paging of
                Nothing ->
                    -- use the API default here?
                    ( 1, ( "", "", "" ) )

                Just pag ->
                    ( Maybe.withDefault 1 pag.limit
                    , ( Maybe.withDefault "" pag.max_id
                      , Maybe.withDefault "" pag.min_id
                      , Maybe.withDefault "" pag.since_id
                      )
                    )

        pagingInput =
            model.pagingInput
    in
    if not model.smartPaging then
        model

    else if since_id /= "" then
        model

    else if min_id /= "" then
        if max_id /= "" then
            model

        else
            case List.head entities of
                Nothing ->
                    model

                Just e ->
                    { model | pagingInput = { pagingInput | min_id = getid e } }

    else if limit <= List.length entities then
        case LE.last entities of
            Nothing ->
                model

            Just e ->
                { model | pagingInput = Debug.log "pagingInput" { pagingInput | max_id = getid e } }

    else
        model


sendGeneralRequest : (Result Error Response -> Msg) -> Request -> Model -> ( Model, Cmd Msg )
sendGeneralRequest tagger request model =
    case model.renderEnv.loginServer of
        Nothing ->
            model |> withNoCmd

        Just server ->
            let
                rawRequest =
                    Request.requestToRawRequest []
                        { server = server
                        , token = model.token
                        }
                        request
            in
            { model
                | request = Just rawRequest
                , response = Nothing
                , entity = Nothing
                , metadata = Nothing
                , selectedKeyPath = ""
                , selectedKeyValue = ""
            }
                |> withCmd
                    (Request.rawRequestToCmd tagger rawRequest)


saveAuthorization : String -> Authorization -> Model -> ( Model, Cmd Msg )
saveAuthorization server authorization model =
    let
        tokens =
            model.tokens
    in
    { model
        | tokens =
            Dict.insert server
                authorization.token
                tokens
    }
        |> withCmd (putToken server <| Just authorization.token)


serverOption : String -> String -> Html Msg
serverOption currentServer server =
    option
        [ value server
        , selected <| server == currentServer
        ]
        [ text server ]


serverSelect : Model -> Html Msg
serverSelect model =
    let
        currentServer =
            case model.renderEnv.loginServer of
                Nothing ->
                    ""

                Just server ->
                    server
    in
    select [ onInput (GlobalMsg << SetServer) ]
        (option [ value "" ]
            [ text "-- select a server --" ]
            :: (List.map (serverOption currentServer) <| Dict.keys model.tokens)
        )


b : String -> Html msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


type alias StyleProperties =
    { backgroundColor : String
    , color : String
    }


styles :
    { dark : StyleProperties
    , light : StyleProperties
    }
styles =
    { dark =
        { backgroundColor = "#222"
        , color = "#eee"
        }
    , light =
        { backgroundColor = "white"
        , color = "black"
        }
    }


getStyle : Style -> StyleProperties
getStyle style =
    case style of
        DarkStyle ->
            styles.dark

        LightStyle ->
            styles.light


type alias ImageSpec =
    { imageUrl : String
    , linkUrl : String
    , altText : String
    , h : String
    }


imageLink : ImageSpec -> Html Msg
imageLink { imageUrl, linkUrl, altText, h } =
    a
        [ href linkUrl
        , blankTarget
        ]
        [ img
            [ src imageUrl
            , alt altText
            , style "height" h
            , title altText
            ]
            []
        ]


link : String -> String -> Html Msg
link label url =
    a
        [ href url
        , blankTarget
        ]
        [ text label ]


blankTarget : Attribute msg
blankTarget =
    target "_blank"


{-| The Material Design CSS puts paragraph spacing BELOW the paragraph.

Use this to make a paragraph worth of vertical white space.

-}
pspace : Html msg
pspace =
    p [] [ text "" ]


enabledButton : Bool -> Msg -> String -> Html Msg
enabledButton enabled msg label =
    Html.button
        [ onClick msg
        , disabled <| not enabled
        ]
        [ b label ]


button : Msg -> String -> Html Msg
button =
    enabledButton True


view : Model -> Document Msg
view model =
    { title = "Mammudeck"
    , body =
        [ renderDialog model
        , case model.page of
            SplashScreenPage ->
                renderSplashScreen model

            ColumnsPage ->
                Deck.renderColumns deckMsg model (pageSelector True True model.page)
        ]
    }


pageSelector : Bool -> Bool -> Page -> Html Msg
pageSelector showLabel showColumns page =
    span []
        [ if showLabel then
            b "Page: "

          else
            text ""
        , select [ onInput (GlobalMsg << SetPage) ]
            [ if showColumns then
                option
                    [ value "ColumnsPage"
                    , selected <| page == ColumnsPage
                    ]
                    [ text "Columns" ]

              else
                text ""
            , option
                [ value "SplashScreenPage"
                , selected <| page == SplashScreenPage
                ]
                [ text "Splash Screen" ]
            ]
        ]


renderCenteredScreen : Model -> String -> List (Html msg) -> Html msg
renderCenteredScreen model width body =
    let
        { backgroundColor, color } =
            getStyle model.renderEnv.style
    in
    div
        [ style "background-color" backgroundColor
        , style "padding" "0 0 0 0"
        , style "margin" "0"
        , style "width" "auto"
        ]
        [ div
            [ style "color" color
            , style "background-color" backgroundColor
            , style "padding" "0 5px 0 5px"
            , style "max-width" "fill-available"
            , style "width" width
            , style "margin" "auto"
            ]
            body
        ]


renderSplashScreen : Model -> Html Msg
renderSplashScreen model =
    renderCenteredScreen model
        "40em"
        [ h2 [ style "text-align" "center" ]
            [ text "Mammudeck" ]
        , pageSelector True (model.renderEnv.loginServer /= Nothing) model.page
        , if model.renderEnv.loginServer == Nothing then
            p []
                [ text "Enter a 'server' name and click 'Login' or 'Set Server'."
                ]

          else
            primaryServerLine model
        , loginSelectedUI model
        , Markdown.toHtml []
            """
Mammudeck is a TweetDeck-like columnar interface to Mastodon/Pleroma. It is a work in progress. Keep an eye on the "Columns" page for new features.

[Wikipedia says](https://en.wikipedia.org/wiki/Mastodon) that "Mastodons... are any species of extinct proboscideans in the genus Mammut (family Mammutidae), distantly related to elephants..." I removed the ending "t" from "Mammut" and added "deck" to get "Mammudeck".

There's a huge list of servers at [fediverse.network](https://fediverse.network/). This webapp doesn't know how to register a new account (yet), so you'll have to do that on the server's web site, then come back here to log in.
            """
        , p [ style "text-align" "center" ]
            [ img
                [ src "images/mammoth-500x360.png"
                , style "width" "500"
                , style "height" "360"
                , alt "Mammoth"
                ]
                []
            ]
        , p [ style "text-align" "center" ]
            [ link "@imacpr0n@mastodon.social"
                "https://mastodon.social/@imacpr0n"
            , br
            , link "@billstclair@kiwifarms.cc"
                "https://kiwifarms.cc/billstclair"
            , br
            , text <| "Copyright " ++ special.copyright ++ " 2019, Bill St. Clair"
            , br
            , imageLink
                { imageUrl = "images/elm-logo-125x125.png"
                , linkUrl = "https://elm-lang.org/"
                , altText = "Elm Inside"
                , h = "32px"
                }
            , text " "
            , imageLink
                { imageUrl =
                    if model.renderEnv.style == DarkStyle then
                        "images/GitHub-Mark-Light-32px.png"

                    else
                        "images/GitHub-Mark-32px.png"
                , linkUrl = "https://github.com/billstclair/mammudeck"
                , altText = "GitHub"
                , h = "32px"
                }
            ]
        ]


feedTitle : FeedType -> Html Msg
feedTitle feedType =
    case feedType of
        HomeFeed ->
            b "Home"

        UserFeed { username } ->
            b <| "User: " ++ username

        PublicFeed _ ->
            b "Public"

        NotificationFeed _ ->
            b "Notifications"

        _ ->
            text ""


hrpct : Int -> Html msg
hrpct pct =
    Html.hr [ style "width" <| String.fromInt pct ++ "%" ] []


hr : Html msg
hr =
    hrpct 90


primaryServerLine : Model -> Html Msg
primaryServerLine model =
    case model.renderEnv.loginServer of
        Nothing ->
            text ""

        Just server ->
            p []
                [ case model.account of
                    Nothing ->
                        b "Using server: "

                    Just account ->
                        span []
                            [ b "Logged in as: "
                            , link ("@" ++ account.username) account.url
                            , text "@"
                            ]
                , link server <| "https://" ++ server
                , text " "
                , button (GlobalMsg Logout) "Logout"
                ]


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (JD.map tagger keyCode)


loginSelectedUI : Model -> Html Msg
loginSelectedUI model =
    p []
        [ pspace
        , b "server: "
        , input
            [ size 30
            , onInput (GlobalMsg << SetServer)
            , value model.server
            , placeholder "mastodon.social"
            , onKeyUp
                (\code ->
                    if code == 13 then
                        GlobalMsg Login

                    else
                        Noop
                )
            ]
            []
        , text " "
        , serverSelect model
        , br
        , Html.button
            [ onClick (GlobalMsg Login)
            , disabled <| model.server == ""
            ]
            [ b "Login" ]
        , text " "
        , button (GlobalMsg SetLoginServer) "Set Server"
        ]


renderDialog : Model -> Html Msg
renderDialog model =
    case model.dialog of
        NoDialog ->
            text ""

        EditColumnsDialog ->
            editColumnsDialog model


editColumnsDialog : Model -> Html Msg
editColumnsDialog model =
    Dialog.render
        { styles = [ ( "width", "40%" ) ]
        , title = "Edit Columns"
        , content = editColumnDialogRows model
        , actionBar =
            [ button (ColumnsUIMsg DismissDialog) "OK" ]
        }
        True


editColumnDialogRows : Model -> List (Html Msg)
editColumnDialogRows model =
    let
        feedTypes =
            model.feedSetDefinition.feedTypes

        row : List (Html Msg) -> Msg -> Html Msg
        row td1 msg =
            tr []
                [ td [] td1
                , td [] [ button msg "+" ]
                ]
    in
    -- This will change quite a bit when I add multiple-server support
    [ table [] <|
        List.concat
            [ if List.member HomeFeed feedTypes then
                []

              else
                [ row [ b "Home" ] (ColumnsUIMsg <| AddFeedColumn HomeFeed) ]
            , case
                LE.find
                    (\feedType ->
                        case feedType of
                            NotificationFeed _ ->
                                True

                            _ ->
                                False
                    )
                    feedTypes
              of
                Just _ ->
                    []

                Nothing ->
                    [ row [ b "Notifications" ]
                        (ColumnsUIMsg <|
                            AddFeedColumn
                                (NotificationFeed
                                    { accountId = Nothing
                                    , exclusions = []
                                    }
                                )
                        )
                    ]
            , case
                LE.find
                    (\feedType ->
                        case feedType of
                            PublicFeed _ ->
                                True

                            _ ->
                                False
                    )
                    feedTypes
              of
                Just _ ->
                    []

                Nothing ->
                    [ row [ b "Public" ]
                        (ColumnsUIMsg <|
                            AddFeedColumn (PublicFeed { flags = Nothing })
                        )
                    ]
            , [ row
                    [ b "User: "
                    , input
                        [ size 30
                        , onInput (ColumnsUIMsg << UserNameInput)
                        , value model.userNameInput
                        , placeholder <|
                            "username@"
                                ++ Maybe.withDefault "server" model.renderEnv.loginServer
                        ]
                        []
                    ]
                    (ColumnsUIMsg <| AddFeedColumn Types.defaultUserFeedType)
              ]
            ]
    , hrpct 100
    , let
        feedRow feedType =
            tr []
                [ td [] [ feedTitle feedType ]
                , td [] [ button (ColumnsUIMsg <| DeleteFeedColumn feedType) "-" ]
                ]
      in
      table [] <|
        List.map feedRow feedTypes
    ]



---
--- Persistence
---


type alias SavedModel =
    { renderEnv : RenderEnv
    , page : Page
    , token : Maybe String
    , server : String
    , feedSetDefinition : FeedSetDefinition
    , username : String
    , accountId : String
    , accountIds : String
    , showMetadata : Bool
    , q : String
    , resolve : Bool
    , following : Bool
    , groupId : String
    , showReceived : Bool
    , showEntity : Bool
    , whichGroups : WhichGroups
    , followReblogs : Bool
    , onlyMedia : Bool
    , pinned : Bool
    , excludeReplies : Bool
    , excludeReblogs : Bool
    , pagingInput : PagingInput
    , local : Bool
    , hashtag : String
    , listId : String
    , smartPaging : Bool
    , showUpdateCredentials : Bool
    , statusId : String
    , useElmButtonNames : Bool
    , showPostStatus : Bool
    , excludedNotificationTypes : List NotificationType
    , notificationsAccountId : String
    , notificationId : String
    , muteNotifications : Bool
    , groupIds : String
    , offset : String
    , listTitle : String
    , filterId : String
    , filterInput : FilterInput
    , scheduledStatusId : String
    , userNameInput : String
    }


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { renderEnv = model.renderEnv
    , page = model.page
    , token = model.token
    , server = model.server
    , feedSetDefinition = model.feedSetDefinition
    , username = model.username
    , accountId = model.accountId
    , accountIds = model.accountIds
    , showMetadata = model.showMetadata
    , q = model.q
    , resolve = model.resolve
    , following = model.following
    , groupId = model.groupId
    , showReceived = model.showReceived
    , showEntity = model.showEntity
    , whichGroups = model.whichGroups
    , followReblogs = model.followReblogs
    , onlyMedia = model.onlyMedia
    , pinned = model.pinned
    , excludeReplies = model.excludeReplies
    , excludeReblogs = model.excludeReblogs
    , pagingInput = model.pagingInput
    , local = model.local
    , hashtag = model.hashtag
    , listId = model.listId
    , smartPaging = model.smartPaging
    , showUpdateCredentials = model.showUpdateCredentials
    , statusId = model.statusId
    , useElmButtonNames = model.useElmButtonNames
    , showPostStatus = model.showPostStatus
    , excludedNotificationTypes = model.excludedNotificationTypes
    , notificationsAccountId = model.notificationsAccountId
    , notificationId = model.notificationId
    , muteNotifications = model.muteNotifications
    , groupIds = model.groupIds
    , offset = model.offset
    , listTitle = model.listTitle
    , filterId = model.filterId
    , filterInput = model.filterInput
    , scheduledStatusId = model.scheduledStatusId
    , userNameInput = model.userNameInput
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel savedModel model =
    let
        renderEnv =
            model.renderEnv

        savedRenderEnv =
            savedModel.renderEnv
    in
    { model
        | renderEnv =
            { renderEnv
                | loginServer = savedRenderEnv.loginServer
                , style = savedRenderEnv.style
            }
        , page = savedModel.page
        , token = savedModel.token
        , server = savedModel.server
        , feedSetDefinition = savedModel.feedSetDefinition
        , username = savedModel.username
        , accountId = savedModel.accountId
        , accountIds = savedModel.accountIds
        , showMetadata = savedModel.showMetadata
        , q = savedModel.q
        , resolve = savedModel.resolve
        , following = savedModel.following
        , groupId = savedModel.groupId
        , showReceived = savedModel.showReceived
        , showEntity = savedModel.showEntity
        , whichGroups = savedModel.whichGroups
        , followReblogs = savedModel.followReblogs
        , onlyMedia = savedModel.onlyMedia
        , pinned = savedModel.pinned
        , excludeReplies = savedModel.excludeReplies
        , excludeReblogs = savedModel.excludeReblogs
        , pagingInput = savedModel.pagingInput
        , local = savedModel.local
        , hashtag = savedModel.hashtag
        , listId = savedModel.listId
        , smartPaging = savedModel.smartPaging
        , showUpdateCredentials = savedModel.showUpdateCredentials
        , statusId = savedModel.statusId
        , useElmButtonNames = savedModel.useElmButtonNames
        , showPostStatus = savedModel.showPostStatus
        , excludedNotificationTypes = savedModel.excludedNotificationTypes
        , notificationsAccountId = savedModel.notificationsAccountId
        , notificationId = savedModel.notificationId
        , muteNotifications = model.muteNotifications
        , groupIds = savedModel.groupIds
        , offset = savedModel.offset
        , listTitle = savedModel.listTitle
        , filterId = savedModel.filterId
        , filterInput = savedModel.filterInput
        , scheduledStatusId = savedModel.scheduledStatusId
        , userNameInput = savedModel.userNameInput
    }


encodeWhichGroups : WhichGroups -> Value
encodeWhichGroups whichGroups =
    JE.string <|
        case whichGroups of
            Request.MemberGroups ->
                "MemberGroups"

            Request.FeaturedGroups ->
                "FeaturedGroups"

            Request.AdminGroups ->
                "AdminGroups"


whichGroupsDecoder : Decoder WhichGroups
whichGroupsDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "MemberGroups" ->
                        JD.succeed Request.MemberGroups

                    "FeaturedGroups" ->
                        JD.succeed Request.FeaturedGroups

                    "AdminGroups" ->
                        JD.succeed Request.AdminGroups

                    _ ->
                        JD.fail <| "Unknown WhichGroups value: " ++ s
            )


{-| Encode `Paging` into `Value`
-}
encodePagingInput : PagingInput -> Value
encodePagingInput { max_id, since_id, min_id, limit } =
    JE.object
        [ ( "max_id", JE.string max_id )
        , ( "since_id", JE.string since_id )
        , ( "min_id", JE.string min_id )
        , ( "limit", JE.string limit )
        ]


{-| Decode `PagingInput`
-}
pagingInputDecoder : Decoder PagingInput
pagingInputDecoder =
    JD.succeed PagingInput
        |> required "max_id" JD.string
        |> required "since_id" JD.string
        |> required "min_id" JD.string
        |> required "limit" JD.string


encodePage : Page -> Value
encodePage page =
    JE.string <|
        case page of
            SplashScreenPage ->
                "SplashScreenPage"

            ColumnsPage ->
                "ColumnsPage"


stringToPage : String -> Page
stringToPage string =
    case string of
        "ColumnsPage" ->
            ColumnsPage

        _ ->
            SplashScreenPage


pageDecoder : Decoder Page
pageDecoder =
    JD.string
        |> JD.andThen
            (\p ->
                JD.succeed <| stringToPage p
            )


encodeStyle : Style -> Value
encodeStyle style =
    case style of
        DarkStyle ->
            JE.string "DarkStyle"

        LightStyle ->
            JE.string "LightStyle"


styleDecoder : Decoder Style
styleDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "DarkStyle" ->
                        JD.succeed DarkStyle

                    "LightStyle" ->
                        JD.succeed LightStyle

                    _ ->
                        JD.fail <| "Unknown Style: " ++ s
            )


encodeRenderEnv : RenderEnv -> Value
encodeRenderEnv env =
    JE.object
        [ ( "loginServer", ED.encodeMaybe JE.string env.loginServer )
        , ( "style", encodeStyle env.style )
        ]


renderEnvDecoder : Decoder RenderEnv
renderEnvDecoder =
    JD.succeed
        (\loginServer style ->
            { emptyRenderEnv
                | loginServer = loginServer
                , style = style
            }
        )
        |> required "loginServer" (JD.nullable JD.string)
        |> required "style" styleDecoder


encodeSavedModel : SavedModel -> Value
encodeSavedModel savedModel =
    JE.object
        [ ( "renderEnv", encodeRenderEnv savedModel.renderEnv )
        , ( "page", encodePage savedModel.page )
        , ( "token", ED.encodeMaybe JE.string savedModel.token )
        , ( "server", JE.string savedModel.server )
        , ( "feedSetDefinition", MED.encodeFeedSetDefinition savedModel.feedSetDefinition )
        , ( "username", JE.string savedModel.username )
        , ( "accountId", JE.string savedModel.accountId )
        , ( "accountIds", JE.string savedModel.accountIds )
        , ( "showMetadata", JE.bool savedModel.showMetadata )
        , ( "q", JE.string savedModel.q )
        , ( "resolve", JE.bool savedModel.resolve )
        , ( "following", JE.bool savedModel.following )
        , ( "groupId", JE.string savedModel.groupId )
        , ( "showReceived", JE.bool savedModel.showReceived )
        , ( "showEntity", JE.bool savedModel.showEntity )
        , ( "whichGroups", encodeWhichGroups savedModel.whichGroups )
        , ( "followReblogs", JE.bool savedModel.followReblogs )
        , ( "onlyMedia", JE.bool savedModel.onlyMedia )
        , ( "pinned", JE.bool savedModel.pinned )
        , ( "excludeReplies", JE.bool savedModel.excludeReplies )
        , ( "excludeReblogs", JE.bool savedModel.excludeReblogs )
        , ( "pagingInput", encodePagingInput savedModel.pagingInput )
        , ( "local", JE.bool savedModel.local )
        , ( "hashtag", JE.string savedModel.hashtag )
        , ( "listId", JE.string savedModel.listId )
        , ( "smartPaging", JE.bool savedModel.smartPaging )
        , ( "showUpdateCredentials", JE.bool savedModel.showUpdateCredentials )
        , ( "statusId", JE.string savedModel.statusId )
        , ( "useElmButtonNames", JE.bool savedModel.useElmButtonNames )
        , ( "showPostStatus", JE.bool savedModel.showPostStatus )
        , ( "excludedNotificationTypes"
          , JE.list ED.encodeNotificationType savedModel.excludedNotificationTypes
          )
        , ( "notificationsAccountId"
          , JE.string savedModel.notificationsAccountId
          )
        , ( "notificationId", JE.string savedModel.notificationId )
        , ( "muteNotifications", JE.bool savedModel.muteNotifications )
        , ( "groupIds", JE.string savedModel.groupIds )
        , ( "offset", JE.string savedModel.offset )
        , ( "listTitle", JE.string savedModel.listTitle )
        , ( "filterId", JE.string savedModel.filterId )
        , ( "filterInput", encodeFilterInput savedModel.filterInput )
        , ( "scheduledStatusId", JE.string savedModel.scheduledStatusId )
        , ( "userNameInput", JE.string savedModel.userNameInput )
        ]


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> required "renderEnv" renderEnvDecoder
        |> required "page" pageDecoder
        |> optional "token" (JD.nullable JD.string) Nothing
        |> required "server" JD.string
        |> optional "feedSetDefinition"
            MED.feedSetDefinitionDecoder
            Types.defaultFeedSetDefinition
        |> optional "username" JD.string ""
        |> optional "accountId" JD.string ""
        |> optional "accountIds" JD.string ""
        |> optional "showMetadata" JD.bool False
        |> optional "q" JD.string ""
        |> optional "resolve" JD.bool False
        |> optional "following" JD.bool False
        |> optional "groupId" JD.string ""
        |> optional "showReceived" JD.bool True
        |> optional "showEntity" JD.bool False
        |> optional "whichGroups" whichGroupsDecoder Request.MemberGroups
        |> optional "followReblogs" JD.bool True
        |> optional "onlyMedia" JD.bool False
        |> optional "pinned" JD.bool False
        |> optional "excludeReplies" JD.bool False
        |> optional "excludeReblogs" JD.bool False
        |> optional "pagingInput" pagingInputDecoder emptyPagingInput
        |> optional "local" JD.bool False
        |> optional "hashtag" JD.string ""
        |> optional "listId" JD.string ""
        |> optional "smartPaging" JD.bool False
        |> optional "showUpdateCredentials" JD.bool False
        |> optional "statusId" JD.string ""
        |> optional "useElmButtonNames" JD.bool False
        |> optional "showPostStatus" JD.bool False
        |> optional "excludedNotificationTypes" (JD.list ED.notificationTypeDecoder) []
        |> optional "notificationsAccountId" JD.string ""
        |> optional "notificationId" JD.string ""
        |> optional "muteNotifications" JD.bool True
        |> optional "groupIds" JD.string ""
        |> optional "offset" JD.string ""
        |> optional "listTitle" JD.string ""
        |> optional "filterId" JD.string ""
        |> optional "filterInput" filterInputDecoder emptyFilterInput
        |> optional "scheduledStatusId" JD.string ""
        |> optional "userNameInput" JD.string ""


put : String -> Maybe Value -> Cmd Msg
put key value =
    localStorageSend (LocalStorage.put (Debug.log "put" key) value)


get : String -> Cmd Msg
get key =
    localStorageSend (LocalStorage.get <| Debug.log "get" key)


getLabeled : String -> String -> Cmd Msg
getLabeled label key =
    localStorageSend
        (LocalStorage.getLabeled label <|
            Debug.log ("getLabeled " ++ label) key
        )


listKeysLabeled : String -> String -> Cmd Msg
listKeysLabeled label prefix =
    localStorageSend (LocalStorage.listKeysLabeled label prefix)


tokenStorageKey : String -> String
tokenStorageKey server =
    pk.token ++ "." ++ server


tokenStorageKeyServer : String -> String
tokenStorageKeyServer key =
    String.dropLeft (String.length pk.token + 1) key


putToken : String -> Maybe String -> Cmd Msg
putToken server token =
    put (tokenStorageKey server) <| Maybe.map JE.string token


getFeedSetDefinition : Cmd Msg
getFeedSetDefinition =
    get pk.feedSetDefinition


putFeedSetDefinition : FeedSetDefinition -> Cmd Msg
putFeedSetDefinition feedSetDefinition =
    put pk.feedSetDefinition (Just <| MED.encodeFeedSetDefinition feedSetDefinition)


localStoragePrefix : String
localStoragePrefix =
    "mammudeck"


initialFunnelState : PortFunnels.State
initialFunnelState =
    PortFunnels.initialState localStoragePrefix


localStorageSend : LocalStorage.Message -> Cmd Msg
localStorageSend message =
    LocalStorage.send (getCmdPort LocalStorage.moduleName ())
        message
        initialFunnelState.storage


{-| The `model` parameter is necessary here for `PortFunnels.makeFunnelDict`.
-}
getCmdPort : String -> model -> (Value -> Cmd Msg)
getCmdPort moduleName _ =
    PortFunnels.getCmdPort (GlobalMsg << Process) moduleName False


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict
        [ LocalStorageHandler storageHandler
        , WebSocketHandler socketHandler
        ]
        getCmdPort


{-| Persistent storage keys
-}
pk : { model : String, token : String, feedSetDefinition : String }
pk =
    { model = "model"
    , token = "token"
    , feedSetDefinition = "feedSetDefinition"
    }



---
--- Special characters
---


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ Char.fromCode code ]


special : { nbsp : String, copyright : String }
special =
    { nbsp = stringFromCode 160 -- \u00A0
    , copyright = stringFromCode 169 -- \u00A9
    }



-- HACKATHON ###################################################################


deckMsg : Deck.Msg Msg
deckMsg =
    { reloadAllColumns = ColumnsUIMsg ReloadAllColumns
    , showEditColumnsDialog = ColumnsUIMsg ShowEditColumnsDialog
    , dismissDialog = ColumnsUIMsg DismissDialog
    , addFeedColumn = ColumnsUIMsg << AddFeedColumn
    , deleteFeedColumn = ColumnsUIMsg << DeleteFeedColumn
    , userNameInput = ColumnsUIMsg << UserNameInput
    }
