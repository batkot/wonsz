module Assets exposing (logo, makkay, btk, hub, szuro, kuba, mateusz)

import WebpackAsset as WA

logo : String
logo = WA.assetUrl "./static/images/logo.png"

makkay : String
makkay = WA.assetUrl "./static/images/makkay.jpg"

btk : String
btk = WA.assetUrl "./static/images/btk.jpg"

hub : String
hub = WA.assetUrl "./static/images/hubert.jpg"

szuro : String
szuro = WA.assetUrl "./static/images/szuro.jpg"

kuba : String
kuba = WA.assetUrl "./static/images/kuba.jpg"

mateusz : String
mateusz = WA.assetUrl "./static/images/mateusz.jpg"
