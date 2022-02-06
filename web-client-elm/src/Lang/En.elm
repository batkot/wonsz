module Lang.En exposing (dictionary)

import Lang exposing (Dict)

dictionary : Dict
dictionary =
    { loginPlaceholder = "Username"
    , passwordPlaceholder = "Password"
    , loginAction = "Sign in"
    , logoutAction = "Log out"
    , badCredentialsMessage = "Bad credentials"
    , connectionErrorMessage = "Connection problem"

    , loginPageTitle = "sign in"
    , dashboardPageTitle = "dashboard"
    , accountPageTitle = "account"
    , scoreboardPageTitle = "scoreboard"
    , notFoundPageTitle = "not found"

    , notFoundMessage = "Not found"
    , confirmLabel = "Ok"
    , denyLabel = "Cancel"
    , loadErrorMessage = "Can't load"

    , changePasswordAction = "Change password"
    , currentPasswordPlaceholder  = "Current password"
    , newPasswordPlaceholder = "New password"

    , pointLabel = "pt"

    , dashboardTitle = "Scoreboards"
    , noScoreboardsMessage = "No scoreboards, yet"
    , noScoreboardsTipMessage = "Click to create one."
    }
