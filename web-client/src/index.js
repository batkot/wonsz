require('./static/style.scss');

const { requireAssets, environment } = require('./environment');
const assets = requireAssets(environment.assetsBasePath);
const options = {
    assets,
    ... environment
}
const app = require('./Main.purs');

app.main(options)();
