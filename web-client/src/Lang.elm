module Lang exposing (Dict, HasDict)

type alias Dict =
    { loginPlaceholder : String
    , passwordPlaceholder : String
    , loginAction : String
    , logoutAction : String
    , badCredentialsMessage : String
    , connectionErrorMessage : String

    , loginPageTitle : String
    , dashboardPageTitle : String
    , accountPageTitle : String
    , scoreboardPageTitle : String
    , notFoundPageTitle : String

    , notFoundMessage : String
    , confirmLabel : String
    , denyLabel : String
    , loadErrorMessage : String

    , changePasswordAction : String
    , currentPasswordPlaceholder : String
    , newPasswordPlaceholder : String

    , pointLabel : String

    , dashboardTitle : String
    }

type alias HasDict a = { a | dict : Dict }
