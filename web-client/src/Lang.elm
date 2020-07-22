module Lang exposing (Dict, HasDict)

type alias Dict =
    { loginPlaceholder : String
    , passwordPlaceholder : String
    , loginAction : String
    , logoutAction : String
    , badCredentialsMessage : String
    , connectionErrorMessage : String
    , pointLabel : String
    }

type alias HasDict a = { a | dict : Dict } 
