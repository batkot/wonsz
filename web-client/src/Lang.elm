module Lang exposing (Dict, HasDict)

type alias Dict =
    { loginPlaceholder : String
    , passwordPlaceholder : String
    , loginAction : String
    , badCredentialsMessage : String
    , connectionErrorMessage : String
    }

type alias HasDict a = { a | dict : Dict } 
