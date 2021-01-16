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

    , notFoundMessage = "Not found"
    , confirmLabel = "Ok"
    , denyLabel = "Cancel"

    , changePasswordAction = "Change password"
    , currentPasswordPlaceholder  = "Current password"
    , newPasswordPlaceholder = "New password"

    , pointLabel = "pt"
    }
