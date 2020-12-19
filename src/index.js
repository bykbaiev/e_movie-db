require('./index.html');

const Elm = require('./Main.elm').Elm;

const KEY = {
    QUERY: 'query',
    FAVORITE_MOVIES: 'favoriteMovies',
};

const getFromLocalStorage = (name, defaultValue) => {
    if (localStorage && localStorage.getItem) {
        return JSON.parse(localStorage.getItem(name) || defaultValue || null);
    }

    return defaultValue || '';
};

const saveToLocalStorage = (name, value) => {
    if (localStorage && localStorage.setItem) {
        localStorage.setItem(name, JSON.stringify(value));
    }
};

const app = Elm.Main.init({
    node: document.getElementById('main'),
    flags: {
        query: getFromLocalStorage(KEY.QUERY),
        apiToken: process.env.API_TOKEN,
        favoriteMovies: getFromLocalStorage(KEY.FAVORITE_MOVIES, '[]')
    }
});

app.ports.storeQuery.subscribe((query) => {
    saveToLocalStorage(KEY.QUERY, query);
});

app.ports.storeFavoriteMovies.subscribe((movies) => {
    const movieList = [...getFromLocalStorage(KEY.FAVORITE_MOVIES, '[]'), ...movies];
    saveToLocalStorage(KEY.FAVORITE_MOVIES, movieList);

    app.ports.onFavoriteMoviesChange.send(movieList);
});

window.addEventListener('storage', (event) => {
    const { key, newValue } = event;

    if (key === KEY.FAVORITE_MOVIES) {
        app.ports.onFavoriteMoviesChange.send(JSON.parse(newValue));
    }
});
