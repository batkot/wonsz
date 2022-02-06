module Assets exposing 
    ( logo
    , question
    , exclamation
    , singleSnake
    )

import WebpackAsset as WA

logo : String
logo = WA.assetUrl "./static/images/logo.png"

question : String
question = WA.assetUrl "./static/images/snake-question.png"

exclamation : String
exclamation = WA.assetUrl "./static/images/snake-exclamation.png"


singleSnake : String
singleSnake = WA.assetUrl "./static/images/single-snake.png"
