module Http.Extra exposing
    ( HttpRequest
    , makeRequest
    , mapRequest
    , addHeader

    , HttpMethod(..)
    , Url(..)
    , HasBaseUrl
    , execute

    , httpErrorToMessage
    )

import Http

import Json.Decode as JD

type alias HasBaseUrl a = { a | baseUrl : Url }

type HttpRequest a = HttpRequest (RequestConfig a)

type HttpMethod 
    = Get
    | Put
    | Post
    | Delete

type Url = Url String

httpErrorToMessage : Http.Error -> String
httpErrorToMessage err = case err of
    Http.BadUrl x -> "BadUrl: " ++ x
    Http.Timeout -> "Connection Problem"
    Http.NetworkError -> "Connection Problem"
    Http.BadStatus x -> "Bad status: " ++ String.fromInt x
    Http.BadBody x -> "Bad response body: " ++ x


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
        request = makeRequest req.url req.method mappedDecoder req.body
    in List.foldl addHeader request req.headers

makeRequest : Url -> HttpMethod -> JD.Decoder a -> Http.Body -> HttpRequest a
makeRequest url method decoder body = RequestConfig url method decoder body []
    |> HttpRequest 
