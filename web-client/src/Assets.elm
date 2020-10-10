module Assets exposing (logo)

import WebpackAsset as WA

logo : String
logo = WA.assetUrl "./static/images/logo.png"
