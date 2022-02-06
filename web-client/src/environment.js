exports.requireAssets = basePath => {
    return {
        logoUrl: require(`${basePath}/images/logo.png`)
    }
}

exports.environment = {
    appContainerSelector: __APP_CONTAINER_SELECTOR__,
    assetsBasePath: __ASSETS_BASE_PATH__,
}
