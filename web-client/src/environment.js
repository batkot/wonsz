exports.requireAssets = basePath => {
    return {
        logoUrl: require(`${basePath}/images/logo.png`),
        singleSnakeUrl: require(`${basePath}/images/single-snake.png`),
    }
}

exports.environment = {
    appContainerSelector: __APP_CONTAINER_SELECTOR__,
    assetsBasePath: __ASSETS_BASE_PATH__,
    language: __APP_LANG__
}
