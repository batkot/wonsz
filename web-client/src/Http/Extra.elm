module Http.Extra exposing
    ( HttpRequest
    , makeRequest
    , mapRequest
    , addHeader

    , HttpMethod(..)
    , Url(..)
    , execute
    )

import Http

import Json.Decode as JD

type HttpRequest a = HttpRequest (RequestConfig a)

type HttpMethod 
    = Get
    | Put
    | Post
    | Delete

type Url = Url String

unUrl : Url -> String
unUrl (Url x) = x

httpMethodToString : HttpMethod -> String
httpMethodToString m = 
    case m of
        Get -> "GET"
        Put -> "PUT"
        Post -> "POST"
        Delete -> "DELETE"

execute : Url -> HttpRequest a -> Cmd (Result Http.Error a)
execute (Url baseUrl) (HttpRequest req) = 
    Http.request 
        { method = httpMethodToString req.method
        , url = baseUrl ++ unUrl req.url
        , headers = req.headers
        , body = req.body
        , expect = Http.expectJson identity req.responseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

type alias RequestConfig a =
    { url : Url
    , method : HttpMethod
    , responseDecoder : JD.Decoder a
    , body : Http.Body
    , headers : List (Http.Header)
    }

addHeader : Http.Header -> HttpRequest a -> HttpRequest a
addHeader header (HttpRequest req) = HttpRequest 
    { req | headers = List.append req.headers [header] }

mapRequest : (a -> b) -> HttpRequest a -> HttpRequest b
mapRequest f (HttpRequest req) = 
    let mappedDecoder = JD.map f req.responseDecoder
    in makeRequest req.url req.method mappedDecoder req.body

makeRequest : Url -> HttpMethod -> JD.Decoder a -> Http.Body -> HttpRequest a
makeRequest url method decoder body = RequestConfig url method decoder body []
    |> HttpRequest 
