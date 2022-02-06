require('./static/theme.scss');

const { Elm } = require('./Main.elm');
const env = require('./environment.js');
env.authToken = window.localStorage.getItem(env.sessionCacheKey);

const initSettings = 
    { 
        node: document.getElementById('app-container'),
        flags: env
    };
const app = Elm.Main.init(initSettings);

app.ports.setLocalStorageKey.subscribe(function([key, value]) {
    window.localStorage.setItem(key, value);
});

app.ports.dropLocalStorageKey.subscribe(function(key) {
    window.localStorage.removeItem(key);
});

window.addEventListener("storage", function(ev) {
    if (ev.storageArea === window.localStorage) {
        app.ports.storageKeyChanged.send([ev.key, ev.newValue]);
    }
});
