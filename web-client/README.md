# Halogen + Webpack 5 starter kit

Simple setup for [PureScript](https://www.purescript.org) + [Halogen](https://github.com/purescript-halogen/purescript-halogen) development using [Webpack 5](https://webpack.js.org/)

Develop with `npm run dev`. Build with `npm run build`. Modify and have fun.

## Features:
- [Static asset loading](./src/environment.js#L3) via Webpack [Asset Modules](./webpack.prod.config.js#L30-L34)
- [Environment variables](./src/environment.js#L7-L10) via Webpack [`Define Plugin`](./webpack.prod.config.js#L69-L72)
- [Startup parameters](./src/Main.purs#L25-L34) parsing via [argonaut](https://github.com/purescript-contrib/purescript-argonaut)
- [PureScript compilation](./webpack.prod.config.js#L35-L49) via Webpack [purs-loader](https://github.com/ethul/purs-loader)
- [SASS styling](./src/static/style.scss) via Webpack [loaders](./webpack.prod.config.js#L19-L30)
- Webpack [dev server](./webpack.dev.config.js#L68-L74)
- Custom monad [AppT](./src/App.purs) placeholder


