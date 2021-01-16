module Page.Account exposing
    ( init
    , Command
    , Model

    , update
    , view
    )

import Http.Extra as HE

import Html exposing (Html, div, img, text, input, button)
import Html.Extra exposing (spinner, onKey, enter)
import Html.Attributes exposing (src, class, classList, type_, placeholder, value, disabled)
import Html.Events exposing (onClick, onInput)

import Page.NotFound as NF

import Lang exposing (HasDict, Dict)

import Auth exposing (AuthSession)

import Effect as Fx exposing (Fx)
import Effect.Compose as Fx exposing (FxComp)
import Effect.Http exposing (HttpFx(..))
import Effect.Command exposing (CommandFx(..))

import IO.Api exposing (getAccountDetails, AccountDetails, changePassword, ChangePasswordRequest)

type Command
    = ShowAccount AccountId 
    | GotAccount AccountDetails
    | InitPasswordChange
    | ClosePasswordChange
    | CurrentPasswordChanged String
    | NewPasswordChanged String
    | RequestPasswordChange PasswordChangeModel
    | PasswordChangeFailed String
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
    , status : PasswordChangeStatus
    }

type PasswordChangeStatus 
    = InProgress
    | Changing
    | Failure String


newPasswordChange : PasswordChangeModel
newPasswordChange = PasswordChangeModel "" "" InProgress

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

        (ClosePasswordChange, Loaded m) -> 
            Fx.pure <| Loaded { m | passwordChange = Nothing }

        (CurrentPasswordChanged x, Loaded m) -> 
            let newModel = Maybe.map (\p -> { p | currentPassword = x }) m.passwordChange
            in Fx.pure <| Loaded { m | passwordChange = newModel }

        (NewPasswordChanged x, Loaded m) -> 
            let newModel = Maybe.map (\p -> { p | newPassword = x }) m.passwordChange
            in Fx.pure <| Loaded { m | passwordChange = newModel }

        (RequestPasswordChange change, Loaded m) -> 
            let apiCall = changePassword (ChangePasswordRequest change.newPassword change.currentPassword) auth
                        |> HE.mapRequest (always ClosePasswordChange)
                httpFx = Request apiCall (always (PasswordChangeFailed "Error"))
                newModel = Maybe.map (\p -> { p | status = Changing }) m.passwordChange
            in if isPasswordChangeModelInvalid change 
                then Fx.pure (Loaded m)
                else Loaded { m | passwordChange = newModel }
                    |> Fx.addFx httpFx

        (_, _) ->
            Fx.pure model

isPasswordChangeModelInvalid : PasswordChangeModel -> Bool
isPasswordChangeModelInvalid { currentPassword, newPassword } = 
    [currentPassword, newPassword]
    |> List.any String.isEmpty

type alias AccountId = Int

init : AccountId -> Fx (CommandFx Command) Model
init accountId = 
    Loading accountId
    |> Fx.addFx (Raise (ShowAccount accountId))

view : HE.HasBaseUrl (HasDict a) -> Model -> Html Command
view env model = 
    case model of 
        Loading _ -> spinner
        Loaded x -> accountView env x
        Error _ -> NF.view env

accountView : HE.HasBaseUrl (HasDict a) -> AccountModel -> Html Command
accountView { baseUrl, dict } model = 
    let changePswdView = Maybe.map (changePasswordView dict) model.passwordChange
                        |> Maybe.withDefault (changePasswordButton dict)
    in
        div [ class "account-container" ] 
            [ div 
                [ class "avatar" ]
                [ img [ src <| HE.unUrl baseUrl ++ model.details.avatarUrl] [] ]

            , div
                [ class "details" ]
                [ div [ class "name" ] [ text model.details.name ]
                , div [ class "login" ] [ text model.details.login ]
                , changePswdView
                ]
            ]

changePasswordButton : Dict -> Html Command
changePasswordButton dict = 
    div [ class "change-password-btn" 
        , onClick InitPasswordChange ] 
        [ text dict.changePasswordAction ]

changePasswordView : Dict -> PasswordChangeModel -> Html Command
changePasswordView dict model = 
    let isSaving = case model.status of 
                Changing -> True
                _ -> False
    in div [ class "password-change" ] 
        [ input 
            [ type_ "password"
            , placeholder dict.currentPasswordPlaceholder
            , value model.currentPassword
            , disabled isSaving
            , onInput CurrentPasswordChanged
            , onKey enter (RequestPasswordChange model) ] 
            []
        , input 
            [ type_ "password"
            , placeholder dict.currentPasswordPlaceholder
            , placeholder dict.newPasswordPlaceholder
            , value model.newPassword
            , disabled isSaving
            , onInput NewPasswordChanged
            , onKey enter (RequestPasswordChange model) ] 
            []
        , div 
            [ class "buttons" ]
            [ div [ class "btn cancel", onClick ClosePasswordChange ] [ text dict.denyLabel ]
            , div 
                [ classList 
                    [ ("btn save", True) 
                    , ("disabled", isPasswordChangeModelInvalid model)
                    ]
                , onClick (RequestPasswordChange model) ] 
                [ text dict.confirmLabel ]
            ]
        ]
