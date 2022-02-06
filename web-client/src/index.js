require('./static/style.scss');

const { requireAssets, environment } = require('./environment');
const assets = requireAssets(environment.assetsBasePath);
const options = {
    appContainerSelector: environment.appContainerSelector,
    assets: assets
}
const app = require('./Main.purs');

app.main(options)();
