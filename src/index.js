require('./index.html');

const Elm = require('./Main.elm').Elm;

const getFromLocalStorage = (name, defaultValue) => {
    if (localStorage && localStorage.getItem) {
        return localStorage.getItem(name) || defaultValue || '';
    }

    return defaultValue || '';
};

const saveToLocalStorage = (name, value) => {
    if (localStorage && localStorage.setItem) {
        localStorage.setItem(name, value);
    }
};

const app = Elm.Main.init({
    node: document.getElementById('main'),
    flags: {
        query: getFromLocalStorage('query'),
        apiToken: process.env.API_TOKEN,
    }
});

app.ports.storeQuery.subscribe((query) => {
    saveToLocalStorage('query', query);
});
