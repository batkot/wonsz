module Page.Account exposing
    ( init
    , Command
    , Model

    , update
    , view
    )

import Http.Extra as HE

import Html exposing (Html, div, img, text, input)
import Html.Extra exposing (spinner, onKey)
import Html.Attributes exposing (src, class, type_, placeholder, value)
import Html.Events exposing (onClick, onInput)

import Page.NotFound as NF

import Lang exposing (HasDict, Dict)

import Auth exposing (AuthSession)

import Effect as Fx exposing (Fx)
import Effect.Compose as Fx exposing (FxComp)
import Effect.Http exposing (HttpFx(..))
import Effect.Command exposing (CommandFx(..))

import IO.Api exposing (getAccountDetails, AccountDetails)

type Command
    = ShowAccount AccountId 
    | GotAccount AccountDetails
    | InitPasswordChange
    | AbandonPasswordChange
    | CurrentPasswordChanged String
    | NewPasswordChanged String
    | GotError String

type Model
    = Loading AccountId
    | Loaded AccountModel
    | Error String

type alias AccountModel = 
    { details : AccountDetails
    , passwordChange : Maybe PasswordChangeModel
    }

type alias PasswordChangeModel =
    { currentPassword : String
    , newPassword : String
    , error : String 
    }

newPasswordChange : PasswordChangeModel
newPasswordChange = PasswordChangeModel "" "" ""

update : AuthSession -> Command -> Model -> Fx (HttpFx Command) Model
update auth command model =
    case (command, model) of
        (ShowAccount accountId, _) -> 
            let apiCall = getAccountDetails accountId auth
                        |> HE.mapRequest GotAccount
                httpFx = Request apiCall (always (GotError ":("))
            in Loading accountId
                |> Fx.addFx httpFx

        (GotAccount accountDetails, _) ->
            AccountModel accountDetails Nothing
            |> Loaded
            |> Fx.pure

        (GotError error, _) ->
            Fx.pure <| Error error

        (InitPasswordChange, Loaded m) -> 
            Fx.pure <| Loaded { m | passwordChange = Just newPasswordChange }

        (AbandonPasswordChange, Loaded m) -> 
            Fx.pure <| Loaded { m | passwordChange = Nothing }

        (CurrentPasswordChanged x, Loaded m) -> 
            let newModel = Maybe.map (\p -> { p | currentPassword = x }) m.passwordChange
            in Fx.pure <| Loaded { m | passwordChange = newModel }

        (NewPasswordChanged x, Loaded m) -> 
            let newModel = Maybe.map (\p -> { p | newPassword = x }) m.passwordChange
            in Fx.pure <| Loaded { m | passwordChange = newModel }

        (_, _) ->
            Fx.pure model

type alias AccountId = Int

init : AccountId -> Fx (CommandFx Command) Model
init accountId = 
    Loading accountId
    |> Fx.addFx (Raise (ShowAccount accountId))

view : HE.HasBaseUrl (HasDict a) -> Model -> Html Command
view env model = 
    case model of 
        Loading _ -> spinner
        Loaded x -> accountDetailsView env x
        Error _ -> NF.view env

accountDetailsView : HE.HasBaseUrl (HasDict a) -> AccountModel -> Html Command
accountDetailsView { baseUrl, dict } model = 
    let changePswdView = Maybe.map (changePasswordView dict) model.passwordChange
                        |> Maybe.withDefault (changePasswordButton dict)
    in
        div [ class "account-container" ] 
            [ div 
                [ class "avatar" ]
                [ img [ src <| HE.unUrl baseUrl ++ model.details.avatarUrl] [] ]

            , div
                [ class "details" ]
                [ changePswdView
                ]
            ]

changePasswordButton : Dict -> Html Command
changePasswordButton dict = 
    div [ class "change-password-btn" 
        , onClick InitPasswordChange ] 
        [ text dict.changePasswordAction ]

changePasswordView : Dict -> PasswordChangeModel -> Html Command
changePasswordView dict model = 
    div [ class "password-change" ] 
        [ input 
            [ type_ "password"
            , class "current-password"
            , placeholder dict.currentPasswordPlaceholder
            , value model.currentPassword
            , onInput CurrentPasswordChanged
            ]
            -- , onKey enter RequestLogin ] 
            []
        , input 
            [ type_ "password"
            , class "new-password"
            , placeholder dict.currentPasswordPlaceholder
            , placeholder dict.newPasswordPlaceholder
            , value model.newPassword
            , onInput NewPasswordChanged
            ]
            -- , onKey enter RequestLogin ] 
            []
        ]
