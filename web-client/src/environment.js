exports.requireAssets = basePath => {
    return {
        purescriptLogoUrl: require(`${basePath}/images/purescript-logo.svg`)
    }
}

exports.environment = {
    appContainerSelector: __APP_CONTAINER_SELECTOR__,
    assetsBasePath: __ASSETS_BASE_PATH__,
}
