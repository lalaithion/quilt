module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode exposing (..)
import List exposing (..)
import WebSocket exposing (..)


---- MODEL ----


type alias Post =
    { email : String, body : String }


type ModelData
    = LoginPage
        { email : String
        , password : String
        }
    | SignupPage
        { email : String
        , password : String
        , verify : String
        }
    | Timeline { email : String, body : String, posts : List Post }


type alias Model =
    ( ModelData, String )


fine : ModelData -> Model
fine model =
    ( model, "" )


init : ( Model, Cmd Msg )
init =
    ( fine (LoginPage { email = "", password = "" }), Cmd.none )



---- UPDATE ----


type Msg
    = DoLogin
    | DoSignup
    | DoPost
    | GoSignup
    | GoLogin
    | LoginEmail String
    | LoginPassword String
    | SignupEmail String
    | SignupPassword String
    | SignupVerify String
    | PostBody String
    | Null


doNothing : Model -> ( Model, Cmd Msg )
doNothing model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( model, err ) =
    case model of
        LoginPage { email, password } ->
            case msg of
                DoLogin ->
                    login email password

                LoginEmail str ->
                    ( fine
                        (LoginPage
                            { email = str
                            , password = password
                            }
                        )
                    , Cmd.none
                    )

                LoginPassword str ->
                    ( fine
                        (LoginPage
                            { email = email
                            , password = str
                            }
                        )
                    , Cmd.none
                    )

                GoSignup ->
                    ( fine
                        (SignupPage
                            { email = ""
                            , password = ""
                            , verify = ""
                            }
                        )
                    , Cmd.none
                    )

                _ ->
                    doNothing (fine model)

        SignupPage { email, password, verify } ->
            case msg of
                DoSignup ->
                    signup email password verify

                SignupEmail str ->
                    ( fine
                        (SignupPage
                            { email = str
                            , password = password
                            , verify = verify
                            }
                        )
                    , Cmd.none
                    )

                SignupPassword str ->
                    ( fine
                        (SignupPage
                            { email = email
                            , password = str
                            , verify = verify
                            }
                        )
                    , Cmd.none
                    )

                SignupVerify str ->
                    ( fine
                        (SignupPage
                            { email = email
                            , password = password
                            , verify = str
                            }
                        )
                    , Cmd.none
                    )

                GoLogin ->
                    ( fine
                        (LoginPage
                            { email = ""
                            , password = ""
                            }
                        )
                    , Cmd.none
                    )

                _ ->
                    doNothing (fine model)

        Timeline { email, body, posts } ->
            case msg of
                DoPost ->
                    newpost email body posts

                PostBody str ->
                    ( fine
                        (Timeline
                            { email = email
                            , body = str
                            , posts = posts
                            }
                        )
                    , Cmd.none
                    )

                GoLogin ->
                    ( fine
                        (LoginPage
                            { email = ""
                            , password = ""
                            }
                        )
                    , Cmd.none
                    )

                _ ->
                    doNothing (fine model)


loginJson : String -> String -> String
loginJson email password =
    encode 0
        (Json.Encode.object
            [ ( "Login"
              , Json.Encode.object
                    [ ( "username", string email )
                    , ( "password", string password )
                    ]
              )
            ]
        )


login : String -> String -> ( Model, Cmd Msg )
login email password =
    ( fine (Timeline { email = email, body = "", posts = [] })
    , WebSocket.send "ws://127.0.0.1:9876/" (loginJson email password)
    )


addUserJson : String -> String -> String
addUserJson email password =
    encode 0
        (Json.Encode.object
            [ ( "AddUser"
              , Json.Encode.object
                    [ ( "username", string email )
                    , ( "password", string password )
                    ]
              )
            ]
        )


signup : String -> String -> String -> ( Model, Cmd Msg )
signup email password verify =
    if password == verify && password /= "" then
        ( fine (Timeline { email = email, body = "", posts = [] })
        , WebSocket.send "ws://127.0.0.1:9876/" (addUserJson email password)
        )
    else
        doNothing
            ( SignupPage
                { email = ""
                , password = ""
                , verify = ""
                }
            , "Error: The passwords must match"
            )


newpost : String -> String -> List Post -> ( Model, Cmd Msg )
newpost email body posts =
    ( fine
        (Timeline { email = email, body = "", posts = Post body email :: posts })
    , Cmd.none
    )



---- VIEW ----


renderPage : Model -> Html Msg
renderPage ( model, err ) =
    case model of
        LoginPage _ ->
            renderLogin ( model, err )

        SignupPage _ ->
            renderSignup ( model, err )

        Timeline _ ->
            renderTimeline ( model, err )


renderLogin : Model -> Html Msg
renderLogin ( model, err ) =
    case model of
        LoginPage { email, password } ->
            div []
                [ div [ class "form", class "login" ]
                    [ input [ type_ "text", value email, placeholder "Enter Your Email", onInput LoginEmail ] []
                    , input [ type_ "text", value password, placeholder "Enter Your Password", onInput LoginPassword ] []
                    , button [ onClick DoLogin ] [ text "login" ]
                    , button [ onClick GoSignup ] [ text "signup" ]
                    ]
                , div [ class "error" ] [ text err ]
                ]

        -- Wrong function! call someone else
        _ ->
            renderPage ( model, err )


renderSignup : Model -> Html Msg
renderSignup ( model, err ) =
    case model of
        SignupPage { email, password, verify } ->
            div []
                [ div [ class "form", class "signup" ]
                    [ input [ type_ "text", value email, placeholder "Enter Your Email", onInput SignupEmail ] []
                    , input [ type_ "text", value password, placeholder "Enter Your Password", onInput SignupPassword ] []
                    , input [ type_ "text", value verify, placeholder "Verify Your Password", onInput SignupVerify ] []
                    , button [ onClick DoSignup ] [ text "sign up" ]
                    , button [ onClick GoLogin ] [ text "login" ]
                    ]
                , div [ class "error" ] [ text err ]
                ]

        -- Wrong function! call someone else
        _ ->
            renderPage ( model, err )


renderTimeline : Model -> Html Msg
renderTimeline ( model, err ) =
    case model of
        Timeline { email, body, posts } ->
            div []
                [ div [ class "form", class "post" ]
                    [ text email
                    , input [ type_ "text", value body, placeholder "Enter Message", onInput PostBody ] []
                    , button [ onClick DoPost ] [ text "post" ]
                    , button [ onClick GoLogin ] [ text "sign out" ]
                    ]
                , div [ class "error" ] [ text err ]
                , div [ class "timeline" ] (List.map renderPost posts)
                ]

        -- Wrong function! call someone else
        _ ->
            renderPage ( model, err )


renderPost : Post -> Html Msg
renderPost post =
    div [ class "post" ] [ text post.email, text "-", text post.body ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen "http://127.0.0.1:9876/" pass


pass : String -> Msg
pass s =
    Null



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = renderPage
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
