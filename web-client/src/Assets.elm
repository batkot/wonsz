module Assets exposing (logo, notFound, singleSnake)

import WebpackAsset as WA

logo : String
logo = WA.assetUrl "./static/images/logo.png"

notFound : String
notFound = WA.assetUrl "./static/images/not-found.gif"

singleSnake : String
singleSnake = WA.assetUrl "./static/images/single-snake.png"
