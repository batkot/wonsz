module Lang.Pl exposing (dictionary)

import Lang exposing (Dict)

dictionary : Dict
dictionary = 
    { loginPlaceholder = "Użytkownik"
    , passwordPlaceholder = "Hasło"
    , loginAction = "Zaloguj"
    , logoutAction = "Wyloguj"
    , badCredentialsMessage = "Złe dane"
    , connectionErrorMessage = "Brak połączenia"
    }
