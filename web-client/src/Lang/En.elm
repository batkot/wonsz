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
    , pointLabel = "pt"
    }
