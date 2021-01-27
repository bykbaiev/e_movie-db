require('./index.html');

const debounce = (callback, wait, immediate = false) => {
    let timeout = null;
    
    return (...args) => {
      const callNow = immediate && !timeout;
      const next = () => callback(...args);
      
      clearTimeout(timeout)
      timeout = setTimeout(next, wait);
  
      if (callNow) {
        next();
      }
    }
  }

const Elm = require('./Main.elm').Elm;

const KEY = {
    SESSION: 'session',
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

const session = getFromLocalStorage(KEY.SESSION) || {};
const favoriteMovies = session.favoriteMovies || null;

const app = Elm.Main.init({
    node: document.getElementById('main'),
    flags: {
        apiToken: process.env.API_TOKEN || null,
        favoriteMovies,
        year: (new Date()).getFullYear().toString()
    }
});

app.ports.storeSession.subscribe(session => {
    saveToLocalStorage(KEY.SESSION, session);


    setTimeout(() => app.ports.onSessionChange.send(session), 0);
});

app.ports.scrollTop.subscribe(() => {
    window.scrollTo({
        top: 0,
        left: 0,
        behavior: 'smooth'
    });
});

window.addEventListener('storage', (event) => {
    const { key, newValue } = event;

    if (key === KEY.SESSION) {
        app.ports.onSessionChange.send(JSON.parse(newValue));
    }
});

window.addEventListener('scroll', debounce(() => app.ports.onScroll.send(window.scrollY), 300));
