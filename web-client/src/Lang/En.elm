module Lang.En exposing (dictionary)

import Lang exposing (Dict)

dictionary : Dict
dictionary = 
    { loginPlaceholder = "Username"
    , passwordPlaceholder = "Password"
    , loginAction = "Log in"
    , badCredentialsMessage = "Bad credentials"
    , connectionErrorMessage = "Connection problem"
    }
