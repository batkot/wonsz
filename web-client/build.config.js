module.exports = {
    appName: 'app.js',
    indexPath: './src/static/index.html',
    assetsPath: './static',
    entryPoint: './src/index.js',
    pursSources: ['src/**/*.purs'],
    buildPath: 'dist',
    excludes: [ /node_modules/, /\.git/, /output/ ]
}
