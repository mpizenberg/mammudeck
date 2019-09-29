module Deck exposing (Msg, renderColumns)

import Html exposing (Html, a, br, button, div, img, span, table, td, text, tr)
import Html.Attributes exposing (alt, class, disabled, href, src, style, target, title)
import Html.Events exposing (onClick)
import Html.Lazy as Lazy
import Html.Parser as Parser
import Html.Parser.Util as Util
import Iso8601
import List.Extra as LE
import Mammudeck.Types exposing (Feed, FeedElements(..), FeedSet, FeedType(..), GangedNotification, RenderEnv, Style(..), StyleProperties)
import Mastodon.Entity
    exposing
        ( Account
        , Attachment
        , AttachmentType(..)
        , Datetime
        , Notification
        , NotificationType(..)
        , Status
        , UrlString
        , WrappedStatus(..)
        )
import Time exposing (Zone)
import Time.Format as Format
import Time.Format.Config.Configs as Configs


type alias Msg msg =
    { reloadAllColumns : msg
    , showEditColumnsDialog : msg
    , dismissDialog : msg
    , addFeedColumn : FeedType -> msg
    , deleteFeedColumn : FeedType -> msg
    , userNameInput : String -> msg
    }


type alias ImageSpec =
    { imageUrl : String
    , linkUrl : String
    , altText : String
    , h : String
    }


renderColumns : Msg msg -> { a | renderEnv : RenderEnv, feedSet : FeedSet } -> Html msg -> Html msg
renderColumns msg { renderEnv, feedSet } pageSelector =
    let
        leftColumn =
            td [] [ Lazy.lazy3 renderLeftColumn msg renderEnv pageSelector ]

        feedColumn =
            \feed ->
                td [] [ Lazy.lazy2 renderFeed renderEnv feed ]
    in
    table [] [ tr [] (leftColumn :: List.map feedColumn feedSet.feeds) ]


renderLeftColumn : Msg msg -> RenderEnv -> Html msg -> Html msg
renderLeftColumn msg renderEnv pageSelector =
    let
        { color } =
            getStyle renderEnv.style
    in
    div
        [ style "color" color
        , style "width" <| px leftColumnWidth
        , style "padding-top" "5px"
        ]
        [ case renderEnv.loginServer of
            Nothing ->
                text ""

            Just server ->
                span []
                    [ link server <| "https://" ++ server
                    , br [] []
                    ]
        , pageSelector
        , br [] []
        , button msg.reloadAllColumns "reload"
        , br [] []
        , button msg.showEditColumnsDialog "edit"
        , br [] []
        ]


renderFeed : RenderEnv -> Feed -> Html msg
renderFeed renderEnv { feedType, elements } =
    let
        { color } =
            getStyle renderEnv.style

        ( _, h ) =
            renderEnv.windowSize
    in
    div
        [ style "width" <| px columnWidth
        , style "height" <| px (h - 20)
        , style "border" <| "1px solid " ++ color
        ]
        [ div
            [ style "border" <| "1px solid " ++ color
            , style "text-align" "center"
            , style "color" color
            ]
            [ feedTitle feedType ]
        , div
            [ style "height" "calc(100% - 1.4em)"
            , style "overflow-y" "auto"
            , style "overflow-x" "hidden"
            ]
          <|
            case elements of
                StatusElements statuses ->
                    List.map (renderStatus renderEnv) statuses

                NotificationElements notifications ->
                    let
                        gangedNotifications =
                            gangNotifications notifications

                        ( _, _ ) =
                            ( Debug.log "notifications" <| List.length notifications
                            , Debug.log "  ganged" <| List.length gangedNotifications
                            )
                    in
                    List.map (renderGangedNotification renderEnv) gangedNotifications

                _ ->
                    [ text "" ]
        ]


renderStatus : RenderEnv -> Status -> Html msg
renderStatus renderEnv statusIn =
    let
        ( status, account, reblogAccount ) =
            case statusIn.reblog of
                Nothing ->
                    ( statusIn, statusIn.account, Nothing )

                Just (WrappedStatus reblog) ->
                    ( reblog, reblog.account, Just statusIn.account )

        { color } =
            getStyle renderEnv.style

        body =
            case Parser.run status.content of
                Ok nodes ->
                    Util.toVirtualDom nodes

                Err _ ->
                    [ text status.content ]
    in
    div [ style "border" <| "1px solid " ++ color ]
        [ div []
            [ div
                [ class "content"
                , style "color" color
                ]
                [ case reblogAccount of
                    Nothing ->
                        text ""

                    Just acct ->
                        span []
                            [ link acct.display_name acct.url
                            , text " reblogged:"
                            ]
                , renderAccount color
                    renderEnv.here
                    account
                    (Html.b [] [ text account.display_name ])
                    status.created_at
                    status.url
                ]
            , hr
            , div
                [ class "content"
                , style "color" color
                ]
                body
            , div [] <|
                List.map (renderAttachment renderEnv) status.media_attachments
            ]
        ]


renderAttachment : RenderEnv -> Attachment -> Html msg
renderAttachment _ attachment =
    case attachment.type_ of
        ImageAttachment ->
            img
                [ src attachment.preview_url
                , alt "image"
                , style "width" "100%"
                ]
                []

        _ ->
            text ""


renderAccount : String -> Zone -> Account -> Html msg -> Datetime -> Maybe UrlString -> Html msg
renderAccount color zone account description datetime url =
    table []
        [ tr []
            [ td []
                [ imageLink
                    { imageUrl = account.avatar
                    , linkUrl = account.url
                    , altText = ""
                    , h = "3em"
                    }
                ]
            , td [ style "color" color ]
                [ description
                , br [] []
                , link ("@" ++ account.username) account.url
                , br [] []
                , let
                    timeString =
                        formatIso8601 zone datetime
                  in
                  case url of
                    Nothing ->
                        text timeString

                    Just u ->
                        link timeString u
                ]
            ]
        ]


gangNotifications : List Notification -> List GangedNotification
gangNotifications notifications =
    let
        loop : List Notification -> List GangedNotification -> List GangedNotification
        loop tail res =
            case tail of
                [] ->
                    List.reverse res

                car :: cdr ->
                    let
                        id =
                            notificationStatusId car
                    in
                    case
                        LE.find
                            (\gn ->
                                (id == gn.id)
                                    && (car.type_ == gn.notification.type_)
                            )
                            res
                    of
                        Nothing ->
                            loop cdr <|
                                { id = id
                                , notification = car
                                , accounts = [ car.account ]
                                }
                                    :: res

                        Just gn ->
                            loop cdr <|
                                { gn | accounts = car.account :: gn.accounts }
                                    :: List.filter ((/=) gn) res
    in
    loop notifications []


notificationStatusId : Notification -> String
notificationStatusId notification =
    case notification.status of
        Just { id } ->
            id

        Nothing ->
            ""


renderGangedNotification : RenderEnv -> GangedNotification -> Html msg
renderGangedNotification renderEnv gangedNotification =
    let
        notification =
            gangedNotification.notification
    in
    case gangedNotification.accounts of
        account :: others ->
            if others == [] then
                renderNotification renderEnv notification

            else
                renderMultiNotification renderEnv
                    account
                    others
                    notification

        _ ->
            renderNotification renderEnv notification


renderMultiNotification : RenderEnv -> Account -> List Account -> Notification -> Html msg
renderMultiNotification renderEnv account others notification =
    let
        { color } =
            getStyle renderEnv.style

        othersCount =
            List.length others

        display_name =
            account.display_name ++ " and " ++ String.fromInt othersCount ++ " others "

        description =
            notificationDescriptionWithDisplayName display_name notification

        timeString =
            formatIso8601 renderEnv.here notification.created_at
    in
    div
        [ style "border" <| "1px solid" ++ color
        , style "color" color
        , style "padding" "0 3px"
        ]
        [ description
        , Html.br [] []
        , text timeString
        , Html.br [] []
        , List.map
            (\other ->
                imageLink
                    { imageUrl = other.avatar
                    , linkUrl = other.url
                    , altText = other.display_name
                    , h = "1.5em"
                    }
            )
            (account :: others)
            |> List.intersperse (text " ")
            |> span []
        , renderNotificationBody renderEnv notification
        ]


renderNotification : RenderEnv -> Notification -> Html msg
renderNotification renderEnv notification =
    let
        description =
            notificationDescription notification

        { color } =
            getStyle renderEnv.style
    in
    div [ style "border" <| "1px solid" ++ color ]
        [ div []
            [ renderAccount color
                renderEnv.here
                notification.account
                description
                notification.created_at
                Nothing
            , renderNotificationBody renderEnv notification
            ]
        ]


renderNotificationBody : RenderEnv -> Notification -> Html msg
renderNotificationBody renderEnv notification =
    let
        { color } =
            getStyle renderEnv.style
    in
    case notification.status of
        Nothing ->
            text ""

        Just status ->
            let
                body =
                    case Parser.run status.content of
                        Ok nodes ->
                            Util.toVirtualDom nodes

                        Err _ ->
                            [ text status.content ]

                timeString =
                    formatIso8601 renderEnv.here status.created_at

                postLink =
                    case status.url of
                        Nothing ->
                            text timeString

                        Just url ->
                            link timeString url
            in
            div []
                [ hr
                , div
                    [ class "content"
                    , style "color" color
                    ]
                  <|
                    postLink
                        :: body
                , div [] <|
                    List.map (renderAttachment renderEnv) status.media_attachments
                ]


notificationDescription : Notification -> Html msg
notificationDescription notification =
    notificationDescriptionWithDisplayName notification.account.display_name
        notification


notificationDescriptionWithDisplayName : String -> Notification -> Html msg
notificationDescriptionWithDisplayName display_name notification =
    let
        postName =
            if notification.type_ == PollNotification then
                text "poll"

            else
                text "your post"
    in
    case notification.type_ of
        FollowNotification ->
            span [] [ Html.b [] [ text display_name ], text " followed you" ]

        MentionNotification ->
            span [] [ Html.b [] [ text display_name ], text " mentioned you" ]

        ReblogNotification ->
            span [] [ Html.b [] [ text display_name ], text " reblogged ", postName ]

        FavouriteNotification ->
            span [] [ Html.b [] [ text display_name ], text " favorited ", postName ]

        PollNotification ->
            span [] [ Html.b [] [ text display_name ], text "'s ", postName, text " is closed" ]


columnWidth : Int
columnWidth =
    300


leftColumnWidth : Int
leftColumnWidth =
    120


getStyle : Style -> StyleProperties
getStyle style =
    case style of
        DarkStyle ->
            styles.dark

        LightStyle ->
            styles.light


px : Int -> String
px int =
    String.fromInt int ++ "px"


styles : { dark : StyleProperties, light : StyleProperties }
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


hr : Html msg
hr =
    hrpct 90


hrpct : Int -> Html msg
hrpct pct =
    Html.hr [ style "width" <| String.fromInt pct ++ "%" ] []


link : String -> String -> Html msg
link label url =
    a [ href url, target "_blank" ] [ text label ]


button : msg -> String -> Html msg
button =
    enabledButton True


enabledButton : Bool -> msg -> String -> Html msg
enabledButton enabled msg label =
    Html.button [ onClick msg, disabled <| not enabled ]
        [ Html.b [] [ text label ] ]


feedTitle : FeedType -> Html msg
feedTitle feedType =
    case feedType of
        HomeFeed ->
            Html.b [] [ text "Home" ]

        UserFeed { username } ->
            Html.b [] [ text ("User: " ++ username) ]

        PublicFeed _ ->
            Html.b [] [ text "Public" ]

        NotificationFeed _ ->
            Html.b [] [ text "Notifications" ]

        _ ->
            text ""


imageLink : ImageSpec -> Html msg
imageLink { imageUrl, linkUrl, altText, h } =
    a
        [ href linkUrl
        , target "_blank"
        ]
        [ img
            [ src imageUrl
            , alt altText
            , style "height" h
            , title altText
            ]
            []
        ]


formatIso8601 : Zone -> String -> String
formatIso8601 zone iso8601 =
    case Iso8601.toTime iso8601 of
        Err _ ->
            iso8601

        Ok posix ->
            Format.format (Configs.getConfig "en_us")
                "%y%m%d %-H:%M:%S"
                zone
                posix
