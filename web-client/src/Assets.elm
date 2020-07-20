module Assets exposing (elmLogoUrl, logo)

import WebpackAsset as WA

elmLogoUrl : String
elmLogoUrl = WA.assetUrl "./static/images/elm-logo.png"

logo : String
logo = WA.assetUrl "./static/images/logo-tmp.png"
