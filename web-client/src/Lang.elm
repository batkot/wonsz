module Lang exposing (Dict, HasDict)

type alias Dict =
    { loginPlaceholder : String
    , passwordPlaceholder : String
    , loginAction : String
    , logoutAction : String
    , badCredentialsMessage : String
    , connectionErrorMessage : String

    , notFoundMessage : String
    , confirmLabel : String
    , denyLabel : String

    , changePasswordAction : String
    , currentPasswordPlaceholder : String
    , newPasswordPlaceholder : String

    , pointLabel : String
    }

type alias HasDict a = { a | dict : Dict } 
